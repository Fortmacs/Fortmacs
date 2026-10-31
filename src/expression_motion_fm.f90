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
!> Deferred: `forward_group_function', `down_group_function' and
!> `up_group_function' (the defaults are always used), shift selection
!> (the `^' of the interactive specs), the subprogram
!> commands (`beginning_of_subprogram', `end_of_subprogram', `mark_subprogram',
!> `narrow_to_subprogram'), `insert_pair', `delete_pair', `raise_expression',
!> `kill_backward_up_group', `insert_parentheses', `move_past_close_and_reindent',
!> `check_parens', `field_complete', `lisp_complete_symbol'.
module expression_motion_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_USER_ERROR, FM_SCAN_ERROR
    use buffer_fm, only: buffers, cur, transient_mark_mode, buffer_serial_live
    use editfns_fm, only: point, goto_char, point_min, point_max, narrow_to_region, widen, mark_position
    use syntax_fm, only: scan_expressions, scan_groups, forward_comment, backward_prefix_chars, syntax_ppss, &
                         parse_state_type, scan_error_data
    use simple_fm, only: kill_region, set_mark, push_mark
    use data_fm, only: value_eq, add_local_variables_killer
    use lisp_fm, only: nilp
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command
    implicit none(type, external)
    private

    public :: forward_expression, backward_expression, forward_group, backward_group, down_group, backward_up_group, up_group, &
              kill_expression, backward_kill_expression, mark_expression, syms_of_expression_motion, &
              forward_expression_default_function, &
              forward_expression_proc

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
    !> with `=>'; null is nil) and buffers' own values (`set_local',
    !> where null is nil too).
    type, public :: forward_expression_function_type
        procedure(forward_expression_proc), pointer, nopass :: value => null()
        type(forward_expression_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (setq-local); absent is
        !> nil.
        procedure :: set_local => forward_expression_set_local
        !> The value in the current buffer (null for nil).
        procedure :: get => forward_expression_get
        !> Whether the current buffer has its own value.
        procedure :: local_p => forward_expression_local_p
        !> Make the current buffer use the default again.
        procedure :: kill_local => forward_expression_kill_local
    end type forward_expression_function_type

    !> If true, `forward_expression' delegates to this function.
    !> Should take the same arguments and behave similarly to `forward_expression'.
    !> Port of lisp.el forward-sexp-function (its default,
    !> `forward_expression_default_function', is set by `syms_of_expression_motion').
    type(forward_expression_function_type), public :: forward_expression_function

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
    logical function forward_expression_local_p(self)
        class(forward_expression_function_type), intent(in) :: self

        forward_expression_local_p = local_entry(self) > 0
    end function forward_expression_local_p

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
    end subroutine kill_expression_motion_locals

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
    !> If unable to move over an expression, signal `scan_error' with three
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

        call mark_expression(prefix_numeric_value(current_prefix_arg),.not. nilp(current_prefix_arg), .true., status)
    end subroutine cmd_mark_expression

    !> Register the commands.  Not a port.
    subroutine syms_of_expression_motion()
        forward_expression_function%value => forward_expression_default_function
        call add_local_variables_killer(kill_expression_motion_locals)
        commands%forward_expression%run => cmd_forward_expression
        commands%backward_expression%run => cmd_backward_expression
        commands%forward_group%run => cmd_forward_group
        commands%backward_group%run => cmd_backward_group
        commands%down_group%run => cmd_down_group
        commands%up_group%run => cmd_up_group
        commands%backward_up_group%run => cmd_backward_up_group
        commands%kill_expression%run => cmd_kill_expression
        commands%backward_kill_expression%run => cmd_backward_kill_expression
        commands%mark_expression%run => cmd_mark_expression
    end subroutine syms_of_expression_motion
end module expression_motion_fm
