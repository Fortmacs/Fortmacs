! expression_motion_fm.f90 -- moving over and killing balanced expressions.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/lisp.el.
!
! Copyright (C) 1985-1986, 1994, 2000-2026 Free Software Foundation,
! Inc.
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

!> The expression and group commands of lisp.el ("Lisp editing commands ...
!> More-or-less applicable in other modes too"), named expression_motion_fm
!> because lisp_fm is lisp.h.  They move with `scan_expressions' and
!> `scan_groups' (syntax_fm), as forward-sexp-default-function,
!> forward-list-default-function, down-list-default-function and
!> up-list-default-function do.  A `scan_error' becomes a `user_error'
!> when a command is run interactively (INTERACTIVE).  `forward_expression'
!> delegates to `forward_expression_function' (which SMIE modes set).
!>
!> The subprogram commands find subprograms with `beginning_of_subprogram_function'
!> (a `beginning_of_subprogram_function_type': buffers' own procedures),
!> `subprogram_prompt_regexp' and `open_paren_in_column_0_is_subprogram_start',
!> and their ends with `end_of_subprogram_function' (a `function_setting').
!> `insert_pair_table' is an array of `insert_pair_type'.
!>
!> Deferred: `forward_group_function', `down_group_function' and
!> `up_group_function' (the defaults are always used), shift selection
!> (the `^' of how the commands read their arguments), `kill_backward_up_group' (needs
!> thingatpt.el), `move_past_close_and_reindent' (M-); needs lisp-mode.el's
!> `calculate_lisp_indent'); not applicable: `field_complete' and
!> `lisp_complete_symbol' (obsolete; Emacs Lisp completion).
module expression_motion_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_USER_ERROR, FM_SCAN_ERROR, FM_GENERIC_ERROR
    use buffer_fm, only: buffers, cur, transient_mark_mode, buffer_serial_live
    use editfns_fm, only: point, goto_char, point_min, point_max, narrow_to_region, widen, mark_position, is_bob, is_eob, &
                          is_bol, is_eol, line_beginning_position, line_end_position, region_beginning, region_end, &
                          preceding_char, following_char, buffer_substring, delete_region
    use syntax_fm, only: scan_expressions, scan_groups, forward_comment, backward_prefix_chars, syntax_ppss, &
                         parse_state_type, scan_error_data, open_paren_in_column_0_is_subprogram_start, &
                         skip_chars_forward, skip_chars_backward, skip_syntax_backward, char_syntax
    use syntax_propertize_fm, only: syntax_ppss_toplevel_pos
    use simple_fm, only: kill_region, set_mark, push_mark, is_use_region, blink_matching_delay
    use data_fm, only: value_eq, add_local_variables_killer, string_setting, logical_setting, function_setting, &
                       forward_setting
    use lisp_fm, only: value_type, xfixnum, vtag, V_INTEGER
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command, last_command_event, sit_for
    use character_fm, only: CHAR_MODIFIER_MASK
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, delete_char
    use insdel_fm, only: insert, insert_char
    use search_fm, only: looking_at, re_search_forward, re_search_backward, match_end, match_data, set_match_data, &
                         SEARCH_NOERROR, SEARCH_MOVE
    use paragraphs_fm, only: page_delimiter
    implicit none(type, external)
    private

    public :: forward_expression, backward_expression, forward_group, backward_group, down_group, backward_up_group, up_group, &
              kill_expression, backward_kill_expression, mark_expression, syms_of_expression_motion, &
              forward_expression_default_function, &
              forward_expression_proc, beginning_of_subprogram_proc, beginning_of_subprogram, beginning_of_subprogram_raw, &
              beginning_of_subprogram_comments, end_of_subprogram, mark_subprogram, narrow_to_subprogram, insert_pair, &
              insert_parentheses, &
              delete_pair, raise_expression, check_parens, buffer_end

    abstract interface
        !> A `forward_expression_function': move across ARG expressions, as
        !> `forward_expression' does.  Not a port.
        subroutine forward_expression_proc(arg, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine forward_expression_proc
    end interface

    ! One buffer's own `forward_expression_function' (FN null is nil).
    type :: forward_expression_entry_type
        integer(int64) :: serial = 0
        procedure(forward_expression_proc), pointer, nopass :: fn => null()
    end type forward_expression_entry_type

    !> The type of `forward_expression_function': the default `value' (assign
    !> with `=>'; null when unset) and buffers' own values (`set_local',
    !> where null means unset too).
    type, public :: forward_expression_function_type
        procedure(forward_expression_proc), pointer, nopass :: value => null()
        type(forward_expression_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (set_local); absent
        !> clears it.
        procedure :: set_local => forward_expression_set_local
        !> The value in the current buffer (null when unset).
        procedure :: get => forward_expression_get
        !> Whether the current buffer has its own value.
        procedure :: local_p => is_forward_expression_local
        !> Make the current buffer use the default again.
        procedure :: kill_local => forward_expression_kill_local
    end type forward_expression_function_type

    !> If true, `forward_expression' delegates to this function.
    !> Should take the same arguments and behave similarly to `forward_expression'.
    !> Port of lisp.el forward-sexp-function (its default,
    !> `forward_expression_default_function', is set by `syms_of_expression_motion').
    type(forward_expression_function_type), public :: forward_expression_function

    abstract interface
        !> A `beginning_of_subprogram_function': move to the beginning of the
        !> ARGth subprogram as `beginning_of_subprogram' does, FOUND if it found one.
        recursive subroutine beginning_of_subprogram_proc(arg, found, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine beginning_of_subprogram_proc
    end interface

    ! One buffer's own `beginning_of_subprogram_function' (FN null is nil).
    type :: beginning_of_subprogram_entry_type
        integer(int64) :: serial = 0
        procedure(beginning_of_subprogram_proc), pointer, nopass :: fn => null()
    end type beginning_of_subprogram_entry_type

    !> The type of `beginning_of_subprogram_function': the default `value'
    !> (assign with `=>'; null when unset) and buffers' own values
    !> (`set_local').
    type, public :: beginning_of_subprogram_function_type
        procedure(beginning_of_subprogram_proc), pointer, nopass :: value => null()
        type(beginning_of_subprogram_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (set_local); absent is
        !> clears it.
        procedure :: set_local => bod_set_local
        !> The value in the current buffer (null when unset).
        procedure :: get => bod_get
        !> Make the current buffer use the default again.
        procedure :: kill_local => bod_kill_local
    end type beginning_of_subprogram_function_type

    !> If true, function for `beginning_of_subprogram_raw' to call.
    !> This is used to find the beginning of the subprogram instead of using the
    !> normal recipe (see `beginning_of_subprogram').  Major modes can define this
    !> if defining `subprogram_prompt_regexp' is not sufficient to handle the mode's
    !> needs.
    !>
    !> The function takes the same argument as `beginning_of_subprogram' and should
    !> behave similarly, returning true if it found the beginning of a subprogram.
    !> Ideally it should move to a point right before an open-paren which encloses
    !> the body of the subprogram.  Port of lisp.el beginning-of-defun-function.
    type(beginning_of_subprogram_function_type), public :: beginning_of_subprogram_function

    !> If true, a regexp to ignore before a subprogram.
    !> This is only necessary if the opening paren or brace is not in column 0.
    !> See function `beginning_of_subprogram'.  Port of lisp.el defun-prompt-regexp
    !> (buffer-local when set).
    type(string_setting), target, public :: subprogram_prompt_regexp = string_setting(name="subprogram_prompt_regexp")

    !> If true, add whitespace as needed when inserting parentheses.
    !> This affects `insert_parentheses' and `insert_pair'.  Port of lisp.el
    !> parens-require-spaces.
    type(logical_setting), target, public :: parens_require_spaces = &
                                             logical_setting(name="parens_require_spaces", value=.true.)

    !> Function for `end_of_subprogram' to call.
    !> This is used to find the end of the subprogram at point.
    !> It is called with no argument, right after calling `beginning_of_subprogram_raw'.
    !> So the function can assume that point is at the beginning of the subprogram body.
    !> It should move point to the first position after the subprogram.  Port of
    !> lisp.el end-of-defun-function.
    type(function_setting), target, public :: end_of_subprogram_function = function_setting(name="end_of_subprogram_function")

    !> Whether `end_of_subprogram' moves to eol before doing anything else.
    !> Set this to .false. if this movement adversely affects the buffer's
    !> major mode's decisions about context.  Port of lisp.el
    !> end-of-defun-moves-to-eol.
    type(logical_setting), target, public :: end_of_subprogram_moves_to_eol = &
                                             logical_setting(name="end_of_subprogram_moves_to_eol", value=.true.)

    !> If true, `narrow_to_subprogram' will also show comments preceding the subprogram.
    !> Port of lisp.el narrow-to-defun-include-comments.
    type(logical_setting), target, public :: narrow_to_subprogram_include_comments = &
                                             logical_setting(name="narrow_to_subprogram_include_comments", value=.false.)

    !> An entry of `insert_pair_table': the characters OPEN and CLOSE, and
    !> the COMMAND character that triggers them (0 if the key is OPEN).
    type, public :: insert_pair_type
        integer(int32) :: command = 0
        integer(int32) :: open = 0
        integer(int32) :: close = 0
    end type insert_pair_type

    !> Table of paired characters inserted by `insert_pair'.
    !> Each element looks like (OPEN-CHAR CLOSE-CHAR) or (COMMAND-CHAR
    !> OPEN-CHAR CLOSE-CHAR).  The characters OPEN-CHAR and CLOSE-CHAR
    !> of the entry whose key is equal to the last input character with
    !> or without modifiers, are inserted by `insert_pair'.
    !>
    !> If COMMAND-CHAR is specified, it is a character that triggers the
    !> insertion of the open/close pair, and COMMAND-CHAR itself isn't
    !> inserted.  Port of lisp.el insert-pair-alist.
    type(insert_pair_type), allocatable, public :: insert_pair_table(:)

    !> Time in seconds to delay after showing a paired character to delete.
    !> It's used by the command `delete_pair'.  The value 0 disables blinking.
    !> Port of lisp.el delete-pair-blink-delay (unset: `blink_matching_delay').
    real, allocatable, public :: delete_pair_blink_delay

    !> True means `delete_pair' pushes mark at end of delimited region.
    !> Port of lisp.el delete-pair-push-mark.
    type(logical_setting), target, public :: delete_pair_push_mark = &
                                             logical_setting(name="delete_pair_push_mark", value=.false.)

contains

    ! Not a port.
    subroutine forward_expression_set_local(self, fn)
        class(forward_expression_function_type), intent(inout) :: self
        procedure(forward_expression_proc), optional :: fn
        type(forward_expression_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = local_entry(self)

        if (i == 0) then
            do i = 1, size(self%entries)
                if (.not. buffer_serial_live(self%entries(i)%serial)) exit
            end do

            if (i > size(self%entries)) then
                n = size(self%entries)
                allocate (grown(n + 1))

                do i = 1, n
                    grown(i)%serial = self%entries(i)%serial
                    grown(i)%fn => self%entries(i)%fn
                end do

                call move_alloc(grown, self%entries)
                i = n + 1
            end if

            self%entries(i)%serial = buffers(cur)%serial
        end if

        if (present(fn)) then
            self%entries(i)%fn => fn
        else
            self%entries(i)%fn => null()
        end if
    end subroutine forward_expression_set_local

    ! The index of the current buffer's entry, 0 if none.  Not a port.
    integer function local_entry(self) result(i)
        class(forward_expression_function_type), intent(in) :: self

        if (cur == 0 .or. .not. allocated(self%entries)) then
            i = 0
            return
        end if

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) return
        end do

        i = 0
    end function local_entry

    ! Not a port.
    function forward_expression_get(self) result(fn)
        class(forward_expression_function_type), intent(in) :: self
        procedure(forward_expression_proc), pointer :: fn
        integer :: i

        i = local_entry(self)

        if (i > 0) then
            fn => self%entries(i)%fn
        else
            fn => self%value
        end if
    end function forward_expression_get

    ! Not a port.
    logical function is_forward_expression_local(self)
        class(forward_expression_function_type), intent(in) :: self

        is_forward_expression_local = local_entry(self) > 0
    end function is_forward_expression_local

    ! Not a port.
    subroutine forward_expression_kill_local(self)
        class(forward_expression_function_type), intent(inout) :: self
        integer :: i

        i = local_entry(self)
        if (i == 0) return
        self%entries(i)%serial = 0
        self%entries(i)%fn => null()
    end subroutine forward_expression_kill_local

    ! kill-all-local-variables: forget the current buffer's
    ! `forward_expression_function'.  Not a port.
    subroutine kill_expression_motion_locals()
        call forward_expression_function%kill_local()
        call beginning_of_subprogram_function%kill_local()
    end subroutine kill_expression_motion_locals

    ! Not a port.
    subroutine bod_set_local(self, fn)
        class(beginning_of_subprogram_function_type), intent(inout) :: self
        procedure(beginning_of_subprogram_proc), optional :: fn
        type(beginning_of_subprogram_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = bod_entry(self)

        if (i == 0) then
            do i = 1, size(self%entries)
                if (.not. buffer_serial_live(self%entries(i)%serial)) exit
            end do

            if (i > size(self%entries)) then
                n = size(self%entries)
                allocate (grown(n + 1))

                do i = 1, n
                    grown(i)%serial = self%entries(i)%serial
                    grown(i)%fn => self%entries(i)%fn
                end do

                call move_alloc(grown, self%entries)
                i = n + 1
            end if

            self%entries(i)%serial = buffers(cur)%serial
        end if

        if (present(fn)) then
            self%entries(i)%fn => fn
        else
            self%entries(i)%fn => null()
        end if
    end subroutine bod_set_local

    ! The index of the current buffer's entry, 0 if none.
    ! Not a port.
    integer function bod_entry(self) result(i)
        class(beginning_of_subprogram_function_type), intent(in) :: self

        if (cur == 0 .or. .not. allocated(self%entries)) then
            i = 0
            return
        end if

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) return
        end do

        i = 0
    end function bod_entry

    ! Not a port.
    function bod_get(self) result(fn)
        class(beginning_of_subprogram_function_type), intent(in) :: self
        procedure(beginning_of_subprogram_proc), pointer :: fn
        integer :: i

        i = bod_entry(self)

        if (i > 0) then
            fn => self%entries(i)%fn
        else
            fn => self%value
        end if
    end function bod_get

    ! Not a port.
    subroutine bod_kill_local(self)
        class(beginning_of_subprogram_function_type), intent(inout) :: self
        integer :: i

        i = bod_entry(self)
        if (i == 0) return
        self%entries(i)%serial = 0
        self%entries(i)%fn => null()
    end subroutine bod_kill_local

    !> (buffer-end ARG): the end of the accessible region if ARG is
    !> positive, else its start.  Port of subr.el:buffer-end.
    integer(int64) function buffer_end(arg)
        integer(int64), intent(in) :: arg

        if (arg > 0) then
            buffer_end = point_max()
        else
            buffer_end = point_min()
        end if
    end function buffer_end

    !> Default function for `forward_expression_function'.  Port of
    !> lisp.el:forward-sexp-default-function.
    recursive subroutine forward_expression_default_function(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        pos = scan_expressions(point(), arg, status)
        if (.not. status%ok()) return
        if (pos == 0) pos = buffer_end(arg)
        call goto_char(pos)
        if (arg < 0) call backward_prefix_chars()
    end subroutine forward_expression_default_function

    !> Move forward across one balanced expression.
    !> With ARG, do it that many times.  Negative arg -N means move
    !> backward across N balanced expressions.  This command assumes
    !> point is not in a string or comment.  Calls
    !> `forward_expression_function' to do the work, if that is set.
    !> If unable to move over an expression, a `scan_error' error condition occurs with three
    !> arguments: a message, the start of the obstacle (usually a
    !> parenthesis or group marker of some kind), and end of the
    !> obstacle.  If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:forward-sexp.
    recursive subroutine forward_expression(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        procedure(forward_expression_proc), pointer :: fn

        fn => forward_expression_function%get()

        if (associated(fn)) then
            call fn(arg, status)
        else
            call forward_expression_default_function(arg, status)
        end if

        if (.not. present(interactive)) return

        if (interactive .and. status%code == FM_SCAN_ERROR) then
            if (arg > 0) then
                call set_error(status, FM_USER_ERROR, "No next expression")
            else
                call set_error(status, FM_USER_ERROR, "No previous expression")
            end if
        end if
    end subroutine forward_expression

    !> Move backward across one balanced expression.
    !> With ARG, do it that many times.  Negative arg -N means
    !> move forward across N balanced expressions.
    !> This command assumes point is not in a string or comment.
    !> Uses `forward_expression' to do the work.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:backward-sexp.
    recursive subroutine backward_expression(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive

        call forward_expression(-arg, status, interactive)
    end subroutine backward_expression

    !> Move forward across one balanced group of parentheses.
    !> This command will also work on other parentheses-like expressions
    !> defined by the current language mode.
    !> With ARG, do it that many times.
    !> Negative arg -N means move backward across N groups of parentheses.
    !> This command assumes point is not in a string or comment.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:forward-list (with forward-list-default-function).
    subroutine forward_group(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        integer(int64) :: pos

        pos = scan_groups(point(), arg, 0_int64, status)

        if (status%ok()) then
            if (pos == 0) pos = buffer_end(arg)
            call goto_char(pos)
            return
        end if

        if (.not. present(interactive)) return

        if (interactive .and. status%code == FM_SCAN_ERROR) then
            if (arg > 0) then
                call set_error(status, FM_USER_ERROR, "No next group")
            else
                call set_error(status, FM_USER_ERROR, "No previous group")
            end if
        end if
    end subroutine forward_group

    !> Move backward across one balanced group of parentheses.
    !> This command will also work on other parentheses-like expressions
    !> defined by the current language mode.
    !> With ARG, do it that many times.
    !> Negative arg -N means move forward across N groups of parentheses.
    !> This command assumes point is not in a string or comment.
    !> Uses `forward_group' to do the work.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:backward-list.
    subroutine backward_group(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive

        call forward_group(-arg, status, interactive)
    end subroutine backward_group

    !> Move forward down one level of parentheses.
    !> This command will also work on other parentheses-like expressions
    !> defined by the current language mode.
    !> With ARG, do this that many times.
    !> A negative argument means move backward but still go down a level.
    !> This command assumes point is not in a string or comment.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:down-list (with down-list-default-function).
    subroutine down_group(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(parse_state_type) :: ppss
        integer(int64) :: n, inc, pos

        status = status_type()
        call syntax_ppss(ppss)

        if (ppss%comstr_start /= 0) then
            call set_error(status, FM_USER_ERROR, "This command doesn't work in strings or comments")
            return
        end if

        n = arg
        inc = -1
        if (n > 0) inc = 1

        do while (n /= 0)
            pos = scan_groups(point(), inc, -1_int64, status)
            if (.not. status%ok()) exit
            if (pos == 0) pos = buffer_end(n)
            call goto_char(pos)
            n = n - inc
        end do

        if (.not. present(interactive)) return
        if (interactive .and. status%code == FM_SCAN_ERROR) call set_error(status, FM_USER_ERROR, "At bottom level")
    end subroutine down_group

    !> Move backward out of one level of parentheses.
    !> This command will also work on other parentheses-like expressions
    !> defined by the current language mode.  With ARG, do this that
    !> many times.  A negative argument means move forward but still to
    !> a less deep spot.
    !>
    !> If ESCAPE_STRINGS is true (as it is interactively), move out
    !> of enclosing strings as well.
    !>
    !> If NO_SYNTAX_CROSSING is true (as it is interactively), prefer
    !> to break out of any enclosing string instead of moving to the
    !> start of a group broken across multiple strings.
    !>
    !> On error, location of point is unspecified.
    !> Port of lisp.el:backward-up-list.
    recursive subroutine backward_up_group(arg, status, escape_strings, no_syntax_crossing)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: escape_strings, no_syntax_crossing

        call up_group(-arg, status, escape_strings, no_syntax_crossing)
    end subroutine backward_up_group

    !> Move forward out of one level of parentheses.
    !> This command will also work on other parentheses-like expressions
    !> defined by the current language mode.  With ARG, do this that
    !> many times.  A negative argument means move backward but still to
    !> a less deep spot.
    !>
    !> If ESCAPE_STRINGS is true (as it is interactively), move out
    !> of enclosing strings as well.
    !>
    !> If NO_SYNTAX_CROSSING is true (as it is interactively), prefer
    !> to break out of any enclosing string instead of moving to the
    !> end of a group broken across multiple strings.
    !>
    !> On error, location of point is unspecified.
    !> Port of lisp.el:up-list (with up-list-default-function).
    recursive subroutine up_group(arg, status, escape_strings, no_syntax_crossing)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: escape_strings, no_syntax_crossing
        type(parse_state_type) :: syntax
        type(status_type) :: st
        integer(int64) :: n, inc, pos, savedb, savedz, opoint, limit
        logical :: escape, crossing, handled, have_syntax, ok, default_fn
        procedure(forward_expression_proc), pointer :: fn
        character(len=:), allocatable :: message
        integer(int64) :: beg, end
        character(len=24) :: buf

        escape = .false.
        if (present(escape_strings)) escape = escape_strings
        crossing = .false.
        if (present(no_syntax_crossing)) crossing = no_syntax_crossing
        status = status_type()
        n = arg
        inc = -1
        if (n > 0) inc = 1

        do while (n /= 0)
            ! (save-restriction ...)
            savedb = buffers(cur)%begv
            savedz = buffers(cur)%text%z - buffers(cur)%zv

            ! If we've been asked not to cross string boundaries and we're
            ! inside a string, narrow to that string so that scan-lists
            ! doesn't find a match in a different string.
            if (crossing) then
                call syntax_ppss(syntax)

                if (syntax%comstr_start /= 0) then
                    opoint = point()
                    call goto_char(syntax%comstr_start)

                    if (syntax%instring /= -1) then
                        ! In string.
                        call forward_expression(1_int64, st)

                        if (st%code == FM_SCAN_ERROR) then
                            limit = point_max()
                        else
                            limit = point()
                        end if
                    else
                        ok = forward_comment(1_int64)
                        limit = point()
                    end if

                    call narrow_to_region(syntax%comstr_start, limit, st)
                    call goto_char(opoint)
                end if
            end if

            fn => forward_expression_function%get()
            default_fn = .not. associated(fn)
            if (.not. default_fn) default_fn = associated(fn, forward_expression_default_function)

            if (default_fn) then
                pos = scan_groups(point(), inc, 1_int64, status)

                if (status%ok()) then
                    if (pos == 0) pos = buffer_end(n)
                    call goto_char(pos)
                end if
            else
                do
                    pos = point()
                    call forward_expression(inc, status)
                    if (.not. status%ok()) exit
                    if (point() == pos) exit
                end do

                if (status%code == FM_SCAN_ERROR) then
                    call scan_error_data(status, message, beg, end)
                    status = status_type()

                    if (n > 0) then
                        call goto_char(end)
                    else
                        call goto_char(beg)
                    end if
                end if

                if (status%ok() .and. point() == pos) then
                    write (buf, "(i0)") point()
                    call set_error(status, FM_SCAN_ERROR, """Unbalanced parentheses"", "//trim(buf)//", "//trim(buf))
                end if
            end if

            call widen()
            call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)

            if (status%code == FM_SCAN_ERROR) then
                handled = .false.
                have_syntax = .false.

                ! If we bumped up against the end of a list, see whether
                ! we're inside a string: if so, just go to the beginning or
                ! end of that string.
                if (escape) then
                    call syntax_ppss(syntax)
                    have_syntax = .true.

                    if (syntax%instring /= -1) then
                        call goto_char(syntax%comstr_start)

                        if (inc > 0) then
                            call forward_expression(1_int64, status)
                            if (.not. status%ok()) return
                        end if

                        handled = .true.
                    end if
                end if

                ! If we narrowed to a comment above and failed to escape it,
                ! the error might be our fault, not an indication that we're
                ! out of syntax.  Try again from beginning or end of the
                ! comment.
                if (.not. handled .and. crossing) then
                    if (.not. have_syntax) call syntax_ppss(syntax)

                    if (syntax%incomment /= 0) then
                        call goto_char(syntax%comstr_start)
                        ok = inc < 0
                        if (.not. ok) ok = forward_comment(1_int64)

                        if (ok) then
                            n = n + inc
                            handled = .true.
                        end if
                    end if
                end if

                if (handled) then
                    status = status_type()
                else if (crossing) then
                    ! Assume called interactively; don't signal an error.
                    call set_error(status, FM_USER_ERROR, "At top level")
                    return
                else
                    return
                end if
            else if (.not. status%ok()) then
                return
            end if

            n = n - inc
        end do
    end subroutine up_group

    !> Kill the balanced expression following point.
    !> With ARG, kill that many expressions after point.
    !> Negative arg -N means kill N expressions before point.
    !> This command assumes point is not in a string or comment.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:kill-sexp.
    subroutine kill_expression(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        integer(int64) :: opoint

        opoint = point()
        call forward_expression(arg, status)

        if (status%ok()) then
            call kill_region(opoint, point(), status)
            return
        end if

        if (.not. present(interactive)) return

        if (interactive .and. status%code == FM_SCAN_ERROR) then
            if (arg > 0) then
                call set_error(status, FM_USER_ERROR, "No next expression")
            else
                call set_error(status, FM_USER_ERROR, "No previous expression")
            end if
        end if
    end subroutine kill_expression

    !> Kill the balanced expression preceding point.
    !> With ARG, kill that many expressions before point.
    !> Negative arg -N means kill N expressions after point.
    !> This command assumes point is not in a string or comment.
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:backward-kill-sexp.
    subroutine backward_kill_expression(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive

        call kill_expression(-arg, status, interactive)
    end subroutine backward_kill_expression

    !> Set mark ARG expressions from point or move mark one expression.
    !> With ALLOW_EXTEND false, mark is set ARG expressions from point.
    !> With ARG given (HAVE_ARG) and ALLOW_EXTEND both true (interactively,
    !> with prefix argument), the place to which mark goes is the same
    !> place \[forward_expression] would move to with the same argument; if the
    !> mark is active, it moves ARG expressions from its current position,
    !> otherwise it is set ARG expressions from point.
    !> When invoked interactively without a prefix argument and no active
    !> region, mark moves one expression forward.
    !> When invoked interactively without a prefix argument, and region
    !> is active, mark moves one expression away of point (i.e., forward
    !> if mark is at or after point, back if mark is before point), thus
    !> extending the region by one expression.  Since the direction of region
    !> extension depends on the relative position of mark and point, you
    !> can change the direction by \[exchange_point_and_mark].
    !> This command assumes point is not in a string or comment.
    !> Port of lisp.el:mark-sexp.
    subroutine mark_expression(arg, have_arg, allow_extend, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: have_arg, allow_extend
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, n, target
        logical :: extend

        opoint = point()
        extend = .false.

        if (allow_extend) then
            if (value_eq(last_command, this_command) .and. mark_position() /= 0) then
                extend = .true.
            else if (transient_mark_mode%value .and. buffers(cur)%mark_active) then
                extend = .true.
            end if
        end if

        if (extend) then
            if (have_arg) then
                n = arg
            else if (mark_position() < point()) then
                n = -1
            else
                n = 1
            end if

            call goto_char(mark_position())
            call forward_expression(n, status)
            target = point()
            call goto_char(opoint)

            if (.not. status%ok()) then
                call to_user_error("No more expression to select")
                return
            end if

            call set_mark(target)
        else
            call forward_expression(arg, status)
            target = point()
            call goto_char(opoint)

            if (.not. status%ok()) then
                call to_user_error("No expression to select")
                return
            end if

            call push_mark(target, nomsg=.false., activate=.true.)
        end if

    contains

        !> The `user_error' for a `scan_error' in STATUS: PREMATURE for
        !> "Containing expression ends prematurely", else the error's
        !> message.
        subroutine to_user_error(premature)
            character(len=*), intent(in) :: premature
            character(len=:), allocatable :: message
            integer(int64) :: beg, end

            if (status%code /= FM_SCAN_ERROR) return
            call scan_error_data(status, message, beg, end)

            if (message == "Containing expression ends prematurely") then
                call set_error(status, FM_USER_ERROR, premature)
            else
                call set_error(status, FM_USER_ERROR, message)
            end if
        end subroutine to_user_error
    end subroutine mark_expression

    ! ------------------------------------------------------------------
    ! Subprograms
    ! ------------------------------------------------------------------

    !> Move backward to the beginning of a subprogram.
    !> With ARG, do it that many times.  Negative ARG means move forward
    !> to the ARGth following beginning of subprogram.
    !>
    !> If search is successful, return true (FOUND); point ends up at the
    !> beginning of the line where the search succeeded.  Otherwise, return
    !> .false..
    !>
    !> When `open_paren_in_column_0_is_subprogram_start' is true, a subprogram
    !> is assumed to start where there is a char with open-parenthesis
    !> syntax at the beginning of a line.  If `subprogram_prompt_regexp' is
    !> true, then a string which matches that regexp may also precede
    !> the open-parenthesis.  If `subprogram_prompt_regexp' and
    !> `open_paren_in_column_0_is_subprogram_start' are both unset, this
    !> function instead finds an open-paren at the outermost level.
    !>
    !> If the variable `beginning_of_subprogram_function' is true, its
    !> value is called as a function, with argument ARG, to find the
    !> subprogram's beginning.
    !>
    !> Regardless of the values of `subprogram_prompt_regexp' and
    !> `beginning_of_subprogram_function', point always moves to the
    !> beginning of the line whenever the search is successful.
    !> Port of lisp.el:beginning-of-defun.
    recursive subroutine beginning_of_subprogram(arg, found, status)
        integer(int64), intent(in) :: arg
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        if (value_eq(this_command, value_type(name="beginning_of_subprogram"))) then
            if (.not. value_eq(last_command, value_type(name="beginning_of_subprogram"))) then
                if (.not. (transient_mark_mode%value .and. buffers(cur)%mark_active)) call push_mark()
            end if
        end if

        call beginning_of_subprogram_raw(arg, found, status)
        if (found) call beginning_of_line()
    end subroutine beginning_of_subprogram

    !> Move point to the character that starts a subprogram.
    !> This is identical to function `beginning_of_subprogram', except that point
    !> does not move to the beginning of the line when `subprogram_prompt_regexp'
    !> is true.
    !>
    !> If variable `beginning_of_subprogram_function' is true, its value
    !> is called as a function to find the subprogram's beginning.
    !>
    !> Return true (FOUND) if this function successfully found the beginning
    !> of a subprogram, .false. if it failed to find one.  Port of
    !> lisp.el:beginning-of-defun-raw.
    recursive subroutine beginning_of_subprogram_raw(arg, found, status)
        integer(int64), intent(in) :: arg
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        procedure(beginning_of_subprogram_proc), pointer :: fn
        type(parse_state_type) :: ppss
        integer(int64), allocatable :: md(:)
        integer(int64) :: n, pos, floor, ceiling, encl_pos, begv, zv_from_end
        character(len=:), allocatable :: re
        type(status_type) :: st
        logical :: arg_positive

        status = status_type()
        found = .false.
        fn => beginning_of_subprogram_function%get()

        if (associated(fn)) then
            call fn(arg, found, status)
            return
        end if

        n = arg

        if (allocated(subprogram_prompt_regexp%value) .or. open_paren_in_column_0_is_subprogram_start) then
            if (n < 0 .and. .not. is_eob()) call goto_char(point() + 1)

            if (allocated(subprogram_prompt_regexp%value)) then
                re = "\(?:"//subprogram_prompt_regexp%value//"\)\s("
                if (open_paren_in_column_0_is_subprogram_start) re = "^\s(\|"//re
            else
                re = "^\s("
            end if

            do
                call re_search_backward(re, pos, st, noerror=SEARCH_MOVE, count=n)
                found = st%ok() .and. pos > 0
                if (.not. found) exit
                ! (save-match-data (nth 8 (syntax-ppss)))
                md = match_data()
                call syntax_ppss(ppss)
                call set_match_data(md)
                if (ppss%comstr_start == 0) exit
            end do

            if (found) call goto_char(match_end(0, st) - 1)
            return
        end if

        ! If open-paren-in-column-0-is-defun-start and defun-prompt-regexp
        ! are both nil, column 0 has no significance - so scan forward
        ! from BOB to see how nested point is, then carry on from there.
        if (n == 0) then
            found = .true.
            return
        end if

        floor = point_min()
        ceiling = point_max()
        arg_positive = n > 0
        ! (save-restriction (widen) ...)
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call syntax_ppss(ppss)

        ! Back out of any comment/string, so that encl-pos will always
        ! become nil if we're at top-level.
        if (ppss%comstr_start > 0) then
            call goto_char(ppss%comstr_start)
            call syntax_ppss(ppss)
        end if

        encl_pos = syntax_ppss_toplevel_pos(ppss)
        if (encl_pos > 0) call goto_char(encl_pos)
        if (encl_pos > 0 .and. arg_positive) n = n - 1

        if (encl_pos == 0 .and. .not. arg_positive) then
            if (.not. looking_at("\s(", st)) n = n + 1
        end if

        ! (to catch crazy parens.)
        pos = scan_groups(point(), -n, 0_int64, st)

        if (st%ok() .and. pos > 0) then
            call goto_char(pos)

            if (arg_positive) then
                if (point() >= floor) then
                    found = .true.
                else
                    call goto_char(floor)
                end if
            else
                ! forward to next (, or trigger the c-c
                pos = scan_groups(point(), 1_int64, -1_int64, st)

                if (st%ok() .and. pos > 0) then
                    call goto_char(pos - 1)

                    if (point() <= ceiling) then
                        found = .true.
                    else
                        call goto_char(ceiling)
                    end if
                else
                    call goto_char(ceiling)
                end if
            end if
        else if (arg_positive) then
            call goto_char(floor)
        else
            call goto_char(ceiling)
        end if

        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
    end subroutine beginning_of_subprogram_raw

    !> Return true if the point is in an "emptyish" line.
    !> This means a line that consists entirely of comments and/or
    !> whitespace.  Port of lisp.el:beginning-of-defun--in-emptyish-line-p.
    logical function is_in_emptyish_line() result(emptyish)
        type(parse_state_type) :: ppss
        integer(int64) :: opoint, shortage, eol
        logical :: moved

        opoint = point()
        shortage = forward_line(0_int64)
        call syntax_ppss(ppss)
        emptyish = .false.

        if (ppss%instring < 0) then
            eol = line_end_position()
            if (ppss%incomment /= 0) call goto_char(ppss%comstr_start)
            moved = forward_comment(point_max())
            emptyish = eol < point()
        end if

        call goto_char(opoint)
    end function is_in_emptyish_line

    !> Move to the beginning of ARGth subprogram, including comments.  Port of
    !> lisp.el:beginning-of-defun-comments.
    recursive subroutine beginning_of_subprogram_comments(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(parse_state_type) :: ppss
        integer(int64) :: shortage, moved
        logical :: found, first_line_p, more

        call beginning_of_subprogram(arg, found, status)
        if (.not. status%ok()) return

        do
            first_line_p = forward_line(-1_int64) == -1
            ! (syntax-ppss (line-end-position)) leaves point there.
            call goto_char(line_end_position())
            call syntax_ppss(ppss)

            ! If eol is in a line-spanning comment, skip to comment start.
            do while (ppss%incomment /= 0 .and. ppss%comstr_start < line_beginning_position())
                call goto_char(ppss%comstr_start)
                call goto_char(line_end_position())
                call syntax_ppss(ppss)
            end do

            more = .not. first_line_p

            if (more) then
                ! Check for blank line.
                moved = skip_syntax_backward("-", line_beginning_position())
                more = .not. is_bol()
            end if

            ! Check for non-comment text.
            if (more) more = is_in_emptyish_line()
            if (.not. more) exit
        end do

        if (first_line_p) then
            shortage = forward_line(0_int64)
        else
            shortage = forward_line(1_int64)
        end if
    end subroutine beginning_of_subprogram_comments

    !> The default `end_of_subprogram_function': (forward-sexp 1).
    !> Port of lisp.el:end-of-defun-function (its default value).
    recursive subroutine end_of_subprogram_default(status)
        type(status_type), intent(out) :: status

        call forward_expression(1_int64, status)
    end subroutine end_of_subprogram_default

    ! Call `end_of_subprogram_function'.
    ! Not a port.
    recursive subroutine execute_end_of_subprogram_function(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (associated(end_of_subprogram_function%value)) then
            call end_of_subprogram_function%value(status)
        end if
    end subroutine execute_end_of_subprogram_function

    ! (unless (is_bol) (skip-chars-forward " \t") (if (looking-at "\\s<\\|\n")
    ! (forward-line 1))): a position right after the end of a function
    ! still counts as in it.
    ! Port of lisp.el:end-of-defun (in part).
    subroutine skip_after_subprogram()
        type(status_type) :: st
        integer(int64) :: moved, shortage

        if (is_bol()) return
        moved = skip_chars_forward(" "//achar(9), st)
        if (looking_at("\s<\|"//achar(10), st)) shortage = forward_line(1_int64)
    end subroutine skip_after_subprogram

    !> Move forward to next end of subprogram.
    !> With argument, do it that many times.
    !> Negative argument -N means move back to Nth preceding end of subprogram.
    !>
    !> An end of a subprogram occurs right after the close-parenthesis that
    !> matches the open-parenthesis that starts a subprogram; see function
    !> `beginning_of_subprogram'.
    !>
    !> If variable `end_of_subprogram_function' is true, its value
    !> is called as a function to find the subprogram's end.
    !>
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:end-of-defun.
    recursive subroutine end_of_subprogram(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        integer(int64) :: n, pos, beg
        logical :: success, found

        if (present(interactive)) then
            if (interactive) then
                call end_of_subprogram(arg, status)
                call scan_to_user_error(status)
                return
            end if
        end if

        if (value_eq(this_command, value_type(name="end_of_subprogram"))) then
            if (.not. value_eq(last_command, value_type(name="end_of_subprogram"))) then
                if (.not. (transient_mark_mode%value .and. buffers(cur)%mark_active)) call push_mark()
            end if
        end if

        n = arg
        if (n == 0) n = 1
        pos = point()
        success = .false.
        if (end_of_subprogram_moves_to_eol%value) call end_of_line(1_int64)
        call beginning_of_subprogram_raw(1_int64, found, status)
        if (.not. status%ok()) return
        beg = point()
        call execute_end_of_subprogram_function(status)
        if (.not. status%ok()) return
        if (n <= 1) call skip_after_subprogram()

        if (n > 0) then
            ! Moving forward.
            if (point() > pos) then
                ! We already moved forward by one because we started from
                ! within a function.
                n = n - 1
            else
                ! We started from after the end of the previous function.
                call goto_char(pos)
            end if

            ! At this point, point either didn't move (because we started
            ! in between two subprogram's), or is at the end of a subprogram
            ! (because we started in the middle of a subprogram).
            if (n /= 0) then
                call beginning_of_subprogram_raw(-n, success, status)
                if (.not. status%ok()) return
                if (success) call execute_end_of_subprogram_function(status)
                if (.not. status%ok()) return
            end if
        else if (n < 0) then
            ! Moving backward.
            if (point() < pos) then
                ! We already moved backward because we started from between
                ! two functions.
                n = n + 1
            else
                ! We started from inside a function.
                call goto_char(beg)
            end if

            if (n /= 0) then
                call beginning_of_subprogram_raw(-n, success, status)
                if (.not. status%ok()) return

                if (success) then
                    beg = point()
                    call execute_end_of_subprogram_function(status)
                    if (.not. status%ok()) return
                end if
            end if
        end if

        call skip_after_subprogram()

        do while (n < 0 .and. point() >= pos .and. success)
            ! We intended to move backward, but this ended up not doing so:
            ! Try harder!
            call goto_char(beg)
            call beginning_of_subprogram_raw(-n, success, status)
            if (.not. status%ok()) return

            ! If we successfully moved pass point, or there is no further
            ! subprogram beginnings anymore, stop.
            if (point() >= beg .or. .not. success) then
                n = 0
            else
                beg = point()
                call execute_end_of_subprogram_function(status)
                if (.not. status%ok()) return
                call skip_after_subprogram()
            end if
        end do
    end subroutine end_of_subprogram

    ! A `scan_error' in STATUS as a `user_error' with its message.
    ! Not a port.
    subroutine scan_to_user_error(status)
        type(status_type), intent(inout) :: status
        character(len=:), allocatable :: message
        integer(int64) :: beg, end

        if (status%code /= FM_SCAN_ERROR) return
        call scan_error_data(status, message, beg, end)
        call set_error(status, FM_USER_ERROR, message)
    end subroutine scan_to_user_error

    !> Put mark at end of this subprogram, point at beginning.
    !> The subprogram marked is the one that contains point or follows point.
    !> With positive ARG, mark this and that many next subprograms; with negative
    !> ARG, change the direction of marking.
    !>
    !> If the mark is active, it marks the next or previous subprogram(s) after
    !> the one(s) already marked.
    !>
    !> If INTERACTIVE is true, as it is interactively,
    !> report errors as appropriate for this kind of usage.
    !> Port of lisp.el:mark-defun.
    recursive subroutine mark_subprogram(arg, status, interactive)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        integer(int64) :: n, opoint, beg, end, k, target, moved, shortage
        logical :: found

        if (present(interactive)) then
            if (interactive) then
                call mark_subprogram(arg, status)
                call scan_to_user_error(status)
                return
            end if
        end if

        status = status_type()
        n = arg
        ! There is no `mark-defun-back' function - see
        ! https://lists.gnu.org/r/bug-gnu-emacs/2016-11/msg00079.html
        ! for explanation
        if (value_eq(last_command, value_type(name="mark_subprogram_back"))) n = -n
        if (n < 0) this_command = value_type(name="mark_subprogram_back")

        if (is_use_region()) then
            if (n >= 0) then
                opoint = point()
                call goto_char(mark_position())

                ! change the dotimes below to (end-of-defun arg)
                ! once bug #24427 is fixed
                do k = 1, n
                    call end_of_subprogram(1_int64, status)
                    if (.not. status%ok()) return
                end do

                target = point()
                call goto_char(opoint)
                call set_mark(target)
            else
                call beginning_of_subprogram_comments(-n, status)
                if (.not. status%ok()) return
            end if
        else
            opoint = point()
            call push_mark(opoint)
            ! Try first in this order for the sake of languages with nested
            ! functions where several can end at the same place as with the
            ! offside rule, e.g. Python.
            call beginning_of_subprogram_comments(1_int64, status)
            if (.not. status%ok()) return
            beg = point()
            call end_of_subprogram(1_int64, status)
            if (.not. status%ok()) return
            end = point()

            ! we were before the first subprogram!
            if ((point() <= opoint .and. n > 0) .or. beg == point_min()) then
                ! beginning-of-defun moved back one subprogram so we got the wrong
                ! one.  If ARG < 0, however, we actually want to go back.
                call goto_char(opoint)
                call end_of_subprogram(1_int64, status)
                if (.not. status%ok()) return
                end = point()
                call beginning_of_subprogram_comments(1_int64, status)
                if (.not. status%ok()) return
                beg = point()
            end if

            call goto_char(beg)

            if (n > 0) then
                ! change the dotimes below to (end-of-defun arg)
                ! once bug #24427 is fixed
                do k = 1, n
                    call end_of_subprogram(1_int64, status)
                    if (.not. status%ok()) return
                end do

                end = point()
                call push_mark(end, nomsg=.false., activate=.true.)
                call goto_char(beg)
            else
                call goto_char(beg)

                ! beginning-of-defun behaves strange with zero arg - see
                ! lists.gnu.org/r/bug-gnu-emacs/2017-02/msg00196.html
                if (n /= -1) then
                    call beginning_of_subprogram(-n - 1, found, status)
                    if (.not. status%ok()) return
                end if

                call push_mark(end, nomsg=.false., activate=.true.)
            end if
        end if

        moved = skip_chars_backward("[:space:]"//achar(10), status)
        if (.not. is_bob()) shortage = forward_line(1_int64)
    end subroutine mark_subprogram

    !> Make text outside current subprogram invisible.
    !> The current subprogram is the one that contains point or follows point.
    !> Preceding comments are included if INCLUDE_COMMENTS is true.
    !> Interactively, the behavior depends on `narrow_to_subprogram_include_comments'.
    !> Port of lisp.el:narrow-to-defun.
    recursive subroutine narrow_to_subprogram(include_comments, status)
        logical, intent(in) :: include_comments
        type(status_type), intent(out) :: status
        integer(int64) :: saved, opoint, here, beg, end, pos, shortage, moved
        type(status_type) :: st
        logical :: found, more

        ! (save-excursion)
        saved = point()
        call widen()
        opoint = point()
        ! Finding the start of the function is a bit problematic since
        ! `beginning-of-defun' when we are on the first character of
        ! the function might go to the previous function.
        !
        ! Therefore we first move one character forward and then call
        ! `beginning-of-defun'.  However now we must check that we did
        ! not move into the next function.
        here = point()
        if (.not. is_eol()) call goto_char(point() + 1)
        call beginning_of_subprogram(1_int64, found, status)
        if (.not. status%ok()) return

        if (point() < here) then
            call goto_char(here)
            call beginning_of_subprogram(1_int64, found, status)
            if (.not. status%ok()) return
        end if

        beg = point()
        call end_of_subprogram(1_int64, status)
        if (.not. status%ok()) return
        end = point()

        do while (looking_at("^"//achar(10), st))
            shortage = forward_line(1_int64)
        end do

        if (.not. point() > opoint) then
            ! beginning-of-defun moved back one subprogram
            ! so we got the wrong one.
            call goto_char(opoint)
            call end_of_subprogram(1_int64, status)
            if (.not. status%ok()) return
            end = point()
            call beginning_of_subprogram(1_int64, found, status)
            if (.not. status%ok()) return
            beg = point()
        end if

        if (include_comments) then
            call goto_char(beg)

            ! Move back past all preceding comments (and whitespace).
            if (forward_comment(-1_int64)) then
                more = .true.

                do while (more)
                    more = forward_comment(-1_int64)
                end do

                ! Move forwards past any page breaks within these comments.
                if (allocated(page_delimiter%value)) then
                    if (len(page_delimiter%value) > 0) then
                        do
                            call re_search_forward(page_delimiter%value, pos, st, bound=beg, noerror=SEARCH_NOERROR)
                            if (.not. st%ok() .or. pos <= 0) exit
                        end do
                    end if
                end if

                ! Lastly, move past any empty lines.
                moved = skip_chars_forward("[:space:]"//achar(10), st)
                call beginning_of_line()
                beg = point()
            end if
        end if

        call goto_char(end)
        call re_search_backward("^"//achar(10), pos, st, bound=point() - 1, noerror=SEARCH_NOERROR)
        call narrow_to_region(beg, end, status)
        call goto_char(min(max(saved, point_min()), point_max()))
    end subroutine narrow_to_subprogram

    ! ------------------------------------------------------------------
    ! Pairs
    ! ------------------------------------------------------------------

    ! The character of the last input event without its modifiers
    ! (subr.el's event-basic-type of a character), -1 if not a character.
    ! Not a port.
    integer(int64) function event_basic_char() result(c)
        c = -1
        if (vtag(last_command_event) /= V_INTEGER) return
        c = iand(xfixnum(last_command_event), int(not(CHAR_MODIFIER_MASK), int64))

        if (c < 32) then
            c = c + 96
        else if (c >= iachar("A") .and. c <= iachar("Z")) then
            c = c + 32
        end if
    end function event_basic_char

    !> Enclose following ARG expressions in a pair of OPEN and CLOSE characters.
    !> Leave point after the first character.
    !> A negative ARG encloses the preceding ARG expressions instead.
    !> No argument is equivalent to zero: just insert characters
    !> and leave point between.
    !> If `parens_require_spaces' is true, this command also inserts a space
    !> before and after, depending on the surrounding characters.
    !> If region is active, insert enclosing characters at region boundaries.
    !>
    !> If arguments OPEN and CLOSE are absent, the character pair is found
    !> from the variable `insert_pair_table' according to the last input
    !> character with or without modifiers.  If no character pair is
    !> found in the variable `insert_pair_table', then the last input
    !> character is inserted ARG times.
    !>
    !> This command assumes point is not in a string or comment.
    !> Port of lisp.el:insert-pair (ARG absent for no prefix).
    recursive subroutine insert_pair(status, arg, open, close)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int32), intent(in), optional :: open, close
        integer(int32) :: o, c
        integer(int64) :: n, ev, basic, b, e, opoint, moved
        character :: syn
        integer :: k

        status = status_type()
        o = 0
        c = 0
        if (present(open)) o = open
        if (present(close)) c = close

        if (o == 0 .or. c == 0) then
            ev = -1
            if (vtag(last_command_event) == V_INTEGER) ev = xfixnum(last_command_event)
            basic = event_basic_char()
            k = pair_index(ev)
            if (k == 0) k = pair_index(basic)

            if (k > 0) then
                o = insert_pair_table(k)%open
                c = insert_pair_table(k)%close
            end if
        end if

        if (o == 0 .or. c == 0) then
            n = 1
            if (present(arg)) n = arg

            do k = 1, int(n)
                call insert_char(int(event_basic_char(), int32), status)
                if (.not. status%ok()) return
            end do

            return
        end if

        if (transient_mark_mode%value .and. buffers(cur)%mark_active) then
            call region_end(e, status)
            if (.not. status%ok()) return
            opoint = point()
            call goto_char(e)
            call insert_char(c, status)
            call goto_char(opoint)
            if (.not. status%ok()) return
            call region_beginning(b, status)
            if (.not. status%ok()) return
            call goto_char(b)
            call insert_char(o, status)
            return
        end if

        n = 0
        if (present(arg)) n = arg

        if (n > 0) then
            moved = skip_chars_forward(" "//achar(9), status)
        else if (n < 0) then
            call forward_expression(n, status)
            if (.not. status%ok()) return
            n = -n
        end if

        if (parens_require_spaces%value .and. .not. is_bob()) then
            syn = char_syntax(preceding_char())

            if (syn == "w" .or. syn == "_" .or. syn == char_syntax(c)) then
                call insert(" ", status)
                if (.not. status%ok()) return
            end if
        end if

        call insert_char(o, status)
        if (.not. status%ok()) return
        ! (save-excursion)
        opoint = point()

        if (n /= 0) then
            call forward_expression(n, status)
            if (.not. status%ok()) return
        end if

        call insert_char(c, status)
        if (.not. status%ok()) return

        if (parens_require_spaces%value .and. .not. is_eob()) then
            syn = char_syntax(following_char())
            if (syn == "w" .or. syn == "_" .or. syn == char_syntax(o)) call insert(" ", status)
        end if

        call goto_char(opoint)

    contains

        ! The entry of `insert_pair_table' whose key is the character KEY.
        integer function pair_index(key) result(found)
            integer(int64), intent(in) :: key
            integer :: i

            found = 0
            if (.not. allocated(insert_pair_table)) return

            do i = 1, size(insert_pair_table)
                if (insert_pair_table(i)%command /= 0) then
                    if (insert_pair_table(i)%command == key) found = i
                else if (insert_pair_table(i)%open == key) then
                    found = i
                end if

                if (found > 0) return
            end do
        end function pair_index
    end subroutine insert_pair

    !> Enclose following ARG expressions in parentheses.
    !> Leave point after open-paren.
    !> A negative ARG encloses the preceding ARG expressions instead.
    !> No argument is equivalent to zero: just insert `()' and leave point between.
    !> If `parens_require_spaces' is true, this command also inserts a space
    !> before and after, depending on the surrounding characters.
    !> If region is active, insert enclosing characters at region boundaries.
    !>
    !> This command assumes point is not in a string or comment.
    !> Port of lisp.el:insert-parentheses (ARG absent for no prefix).
    recursive subroutine insert_parentheses(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        call insert_pair(status, arg, int(iachar("("), int32), int(iachar(")"), int32))
    end subroutine insert_parentheses

    !> Delete a pair of characters enclosing ARG expressions that follow point.
    !> A negative ARG deletes a pair around the preceding ARG expressions instead.
    !> The option `delete_pair_blink_delay' can disable blinking.  With
    !> `delete_pair_push_mark' enabled, pushes a mark at the end of the
    !> enclosed region.  Port of lisp.el:delete-pair.
    recursive subroutine delete_pair(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, inner, moved
        integer(int32) :: open_char, close_char
        real :: delay

        status = status_type()
        delay = blink_matching_delay
        if (allocated(delete_pair_blink_delay)) delay = delete_pair_blink_delay
        opoint = point()

        if (arg < 0) then
            moved = skip_chars_backward(" "//achar(9), status)
            inner = point()
            close_char = preceding_char()
            call forward_expression(arg, status)
            if (.not. status%ok()) return

            if (.not. matching_pair(following_char(), close_char)) then
                call set_error(status, FM_GENERIC_ERROR, "Not after matching pair")
                call goto_char(opoint)
                return
            end if

            if (delay > 0) call sit_for(int(delay*1000))
            call delete_char(1_int64, status)
            if (.not. status%ok()) return
            ! The closing character moved back by one.
            call goto_char(inner - 1)
            call delete_char(-1_int64, status)
            opoint = opoint - 2
        else
            moved = skip_chars_forward(" "//achar(9), status)
            inner = point()
            open_char = following_char()
            call forward_expression(arg, status)
            if (.not. status%ok()) return

            if (.not. matching_pair(open_char, preceding_char())) then
                call set_error(status, FM_GENERIC_ERROR, "Not before matching pair")
                call goto_char(opoint)
                return
            end if

            if (delay > 0) call sit_for(int(delay*1000))
            call delete_char(-1_int64, status)
            if (.not. status%ok()) return
            if (delete_pair_push_mark%value) call push_mark()
            call goto_char(inner)
            call delete_char(1_int64, status)
        end if

        call goto_char(min(max(opoint, point_min()), point_max()))

    contains

        ! Whether (OPEN CLOSE) is one of the pairs of `insert_pair_table'.
        logical function matching_pair(open, close)
            integer(int32), intent(in) :: open, close
            integer :: i

            matching_pair = .false.
            if (.not. allocated(insert_pair_table)) return

            do i = 1, size(insert_pair_table)
                if (insert_pair_table(i)%open == open .and. insert_pair_table(i)%close == close) matching_pair = .true.
            end do
        end function matching_pair
    end subroutine delete_pair

    !> Raise N expressions one level higher up the tree.
    !>
    !> This function removes the expression enclosing the form which follows
    !> point, and then re-inserts N expressions that originally followed point,
    !> thus raising those N expressions one level up.
    !>
    !> Interactively, N is the numeric prefix argument, and defaults to 1.
    !>
    !> For instance, if you have:
    !>
    !>   x = max(a, min(b, c))
    !>
    !> and point is before min, \[raise_expression] will give you
    !>
    !>   x = min(b, c)
    !>
    !> Port of lisp.el:raise-sexp.
    recursive subroutine raise_expression(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s
        integer(int64) :: b, e, opoint

        if (transient_mark_mode%value .and. buffers(cur)%mark_active) then
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
        else
            b = point()
            call forward_expression(n, status)
            e = point()
            call goto_char(b)
            if (.not. status%ok()) return
        end if

        call buffer_substring(b, e, s, status)
        if (.not. status%ok()) return
        call backward_up_group(1_int64, status)
        if (.not. status%ok()) return
        opoint = point()
        call forward_expression(1_int64, status)
        e = point()
        call goto_char(opoint)
        if (.not. status%ok()) return
        call delete_region(point(), e, status)
        if (.not. status%ok()) return
        call insert(s, status)
        call goto_char(opoint)
    end subroutine raise_expression

    !> Check for unbalanced parentheses in the current buffer.
    !> More accurately, check the narrowed part of the buffer for unbalanced
    !> expressions in general.  This is done according to the
    !> current syntax table and will find unbalanced brackets or quotes as
    !> appropriate.  (See Info node `(emacs)Parentheses'.)  If imbalance is
    !> found, an error condition occurs and point is left at the first unbalanced
    !> character.  Port of lisp.el:check-parens.
    subroutine check_parens(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: message
        integer(int64) :: pos, beg, end

        ! Buffer can't have more than (point-max) expressions.
        pos = scan_expressions(point_min(), point_max(), status)
        if (status%code /= FM_SCAN_ERROR) return
        call scan_error_data(status, message, beg, end)
        call push_mark()
        call goto_char(beg)
        ! Could print the scan error's message, which is either
        ! "Containing expression ends prematurely" or
        ! "Unbalanced parentheses", but those may not be so
        ! accurate/helpful, e.g. quotes may actually be
        ! mismatched.
        call set_error(status, FM_USER_ERROR, "Unmatched bracket or quote")
    end subroutine check_parens

    subroutine cmd_beginning_of_subprogram(status)
        type(status_type), intent(out) :: status
        logical :: found

        call beginning_of_subprogram(prefix_numeric_value(current_prefix_arg), found, status)
    end subroutine cmd_beginning_of_subprogram

    subroutine cmd_beginning_of_subprogram_raw(status)
        type(status_type), intent(out) :: status
        logical :: found

        call beginning_of_subprogram_raw(prefix_numeric_value(current_prefix_arg), found, status)
    end subroutine cmd_beginning_of_subprogram_raw

    subroutine cmd_beginning_of_subprogram_comments(status)
        type(status_type), intent(out) :: status

        call beginning_of_subprogram_comments(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_beginning_of_subprogram_comments

    subroutine cmd_end_of_subprogram(status)
        type(status_type), intent(out) :: status

        call end_of_subprogram(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_end_of_subprogram

    subroutine cmd_mark_subprogram(status)
        type(status_type), intent(out) :: status

        call mark_subprogram(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_mark_subprogram

    subroutine cmd_narrow_to_subprogram(status)
        type(status_type), intent(out) :: status

        call narrow_to_subprogram(narrow_to_subprogram_include_comments%value, status)
    end subroutine cmd_narrow_to_subprogram

    subroutine cmd_insert_pair(status)
        type(status_type), intent(out) :: status

        if (.not. allocated(current_prefix_arg%a)) then
            call insert_pair(status)
        else
            call insert_pair(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_insert_pair

    subroutine cmd_insert_parentheses(status)
        type(status_type), intent(out) :: status

        if (.not. allocated(current_prefix_arg%a)) then
            call insert_parentheses(status)
        else
            call insert_parentheses(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_insert_parentheses

    subroutine cmd_delete_pair(status)
        type(status_type), intent(out) :: status

        call delete_pair(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_delete_pair

    subroutine cmd_raise_expression(status)
        type(status_type), intent(out) :: status

        call raise_expression(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_raise_expression

    subroutine cmd_forward_expression(status)
        type(status_type), intent(out) :: status

        call forward_expression(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_forward_expression

    subroutine cmd_backward_expression(status)
        type(status_type), intent(out) :: status

        call backward_expression(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_backward_expression

    subroutine cmd_forward_group(status)
        type(status_type), intent(out) :: status

        call forward_group(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_forward_group

    subroutine cmd_backward_group(status)
        type(status_type), intent(out) :: status

        call backward_group(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_backward_group

    subroutine cmd_down_group(status)
        type(status_type), intent(out) :: status

        call down_group(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_down_group

    subroutine cmd_up_group(status)
        type(status_type), intent(out) :: status

        call up_group(prefix_numeric_value(current_prefix_arg), status, .true., .true.)
    end subroutine cmd_up_group

    subroutine cmd_backward_up_group(status)
        type(status_type), intent(out) :: status

        call backward_up_group(prefix_numeric_value(current_prefix_arg), status, .true., .true.)
    end subroutine cmd_backward_up_group

    subroutine cmd_kill_expression(status)
        type(status_type), intent(out) :: status

        call kill_expression(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_kill_expression

    subroutine cmd_backward_kill_expression(status)
        type(status_type), intent(out) :: status

        call backward_kill_expression(prefix_numeric_value(current_prefix_arg), status, .true.)
    end subroutine cmd_backward_kill_expression

    subroutine cmd_mark_expression(status)
        type(status_type), intent(out) :: status

        call mark_expression(prefix_numeric_value(current_prefix_arg), allocated(current_prefix_arg%a), .true., status)
    end subroutine cmd_mark_expression

    !> Register the commands.  Not a port.
    subroutine syms_of_expression_motion()
        forward_expression_function%value => forward_expression_default_function
        call add_local_variables_killer(kill_expression_motion_locals)
        commands%forward_expression%execute => cmd_forward_expression
        commands%backward_expression%execute => cmd_backward_expression
        commands%forward_group%execute => cmd_forward_group
        commands%backward_group%execute => cmd_backward_group
        commands%down_group%execute => cmd_down_group
        commands%up_group%execute => cmd_up_group
        commands%backward_up_group%execute => cmd_backward_up_group
        commands%kill_expression%execute => cmd_kill_expression
        commands%backward_kill_expression%execute => cmd_backward_kill_expression
        commands%mark_expression%execute => cmd_mark_expression
        call forward_setting(subprogram_prompt_regexp, local_if_set=.true.)
        call forward_setting(parens_require_spaces)
        call forward_setting(end_of_subprogram_function)
        call forward_setting(end_of_subprogram_moves_to_eol)
        call forward_setting(narrow_to_subprogram_include_comments)
        call forward_setting(delete_pair_push_mark)
        if (.not. associated(end_of_subprogram_function%value)) end_of_subprogram_function%value => end_of_subprogram_default

        if (.not. allocated(insert_pair_table)) then
            allocate (insert_pair_table(7))
            insert_pair_table(1) = insert_pair_type(open=iachar("("), close=iachar(")"))
            insert_pair_table(2) = insert_pair_type(open=iachar("["), close=iachar("]"))
            insert_pair_table(3) = insert_pair_type(open=iachar("{"), close=iachar("}"))
            insert_pair_table(4) = insert_pair_type(open=iachar("<"), close=iachar(">"))
            insert_pair_table(5) = insert_pair_type(open=iachar('"'), close=iachar('"'))
            insert_pair_table(6) = insert_pair_type(open=iachar("'"), close=iachar("'"))
            insert_pair_table(7) = insert_pair_type(open=iachar("`"), close=iachar("'"))
        end if

        commands%beginning_of_subprogram%execute => cmd_beginning_of_subprogram
        commands%beginning_of_subprogram_raw%execute => cmd_beginning_of_subprogram_raw
        commands%beginning_of_subprogram_comments%execute => cmd_beginning_of_subprogram_comments
        commands%end_of_subprogram%execute => cmd_end_of_subprogram
        commands%mark_subprogram%execute => cmd_mark_subprogram
        commands%narrow_to_subprogram%execute => cmd_narrow_to_subprogram
        commands%insert_pair%execute => cmd_insert_pair
        commands%insert_parentheses%execute => cmd_insert_parentheses
        commands%delete_pair%execute => cmd_delete_pair
        commands%raise_expression%execute => cmd_raise_expression
        commands%check_parens%execute => check_parens
    end subroutine syms_of_expression_motion
end module expression_motion_fm
