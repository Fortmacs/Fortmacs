! fm_simple.f90 -- basic editing commands.
! Ported from GNU Emacs 31.1 lisp/simple.el.
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> Commands from simple.el that the basic key bindings need: newlines,
!> deletion, keyboard-quit, quoted-insert, and the prefix-argument
!> commands with universal-argument-map.
!>
!> Not ported yet: the mark and region (so no `delete-active-region`),
!> undo, the kill ring, electric indentation, auto-fill and hard
!> newlines, prefix-argument echoing, octal input in quoted-insert.
module fm_simple
    use, intrinsic :: iso_fortran_env, only: int64
    use fm_error, only: status_type, set_error, FM_GENERIC_ERROR, FM_QUIT, &
                        FM_USER_ERROR, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER
    use fm_handle, only: handle_type
    use fm_lisp, only: value_type, nil, make_fixnum, list, fixnump, consp, &
                       nilp, xfixnum, list_elt
    use fm_lread, only: sym
    use fm_data, only: value_eq
    use fm_character, only: MAX_CHAR
    use fm_buffer, only: barf_if_buffer_read_only, buffers, cur
    use fm_editfns, only: point, goto_char, point_max, line_beginning_position, line_end_position, &
                          bolp, constrain_to_field, pos_eol
    use fm_textprop, only: get_text_property
    use fm_indent, only: current_column, move_to_column, vertical_motion, screen_column
    use fm_insdel, only: insert, del_range
    use fm_data, only: symbol_value
    use fm_lisp, only: list_length, xstring, stringp
    use fm_minibuf, only: minibuffer_history_position, minibuffer_history_variable, &
                          minibuffer_default, minibuffer_text_before_history, &
                          minibuffer_prompt_end, minibuffer_contents
    use fm_cmds, only: delete_char, self_insert_command, forward_line, end_of_line, beginning_of_line, &
                       backward_char
    use fm_keymap, only: make_sparse_keymap, define_key, keymap_live_p, &
                         key_description
    use fm_callint, only: defcommand, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value
    use fm_keyboard, only: last_command_event, this_command, &
                           real_this_command, last_command, &
                           real_last_command, last_prefix_arg, read_event, &
                           set_transient_map
    implicit none(type, external)
    private

    public :: newline, open_line, delete_backward_char, &
              prefix_command_preserve_state, universal_argument_map, &
              syms_of_simple, goto_history_element, line_move, next_line, previous_line

    !> goal-column: a semipermanent goal column for vertical motion, or
    !> -1 for none.
    integer(int64), public :: goal_column = -1
    !> line-move-visual: move by screen lines.
    logical, public :: line_move_visual = .true.
    !> temporary-goal-column: the column vertical motion tries to keep;
    !> `tgc_visual` when it is a screen x (upstream's (X . HSCROLL) form)
    !> rather than a current-column.
    integer(int64) :: temporary_goal_column = 0
    logical :: tgc_visual = .false.

    type(handle_type) :: uarg_map

contains

    integer(int64) function arg_p()
        arg_p = prefix_numeric_value(current_prefix_arg)
    end function arg_p

    !> Insert `n` newlines.  Port of simple.el:newline (no margins,
    !> electric indentation, auto-fill or hard newlines).
    subroutine newline(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        if (n < 0) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "Repetition argument has to be non-negative")
            return
        end if
        call self_insert_command(n, 10_int64, status)
    end subroutine newline

    !> Insert `n` newlines, leaving point before them.  Port of
    !> simple.el:open-line (no fill prefix or left margin).
    subroutine open_line(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: loc

        loc = point()
        call newline(n, status)
        if (.not. status%ok()) return
        call goto_char(loc)
    end subroutine open_line

    !> Delete `n` characters before point.  Port of
    !> simple.el:delete-backward-char (no region, untabify or kill).
    subroutine delete_backward_char(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call delete_char(-n, status)
    end subroutine delete_backward_char

    !> Pass the prefix-argument state on to the next command, which keeps
    !> `last_command` too.  Port of simple.el:prefix-command-preserve-state
    !> with universal-argument--preserve as its hook.
    subroutine prefix_command_preserve_state()
        prefix_arg = current_prefix_arg
        current_prefix_arg = last_prefix_arg
        this_command = last_command
        real_this_command = real_last_command
    end subroutine prefix_command_preserve_state

    !> The keymap active after C-u: digits, minus and C-u build the
    !> argument.  Port of simple.el:universal-argument-map (`-` is a
    !> command here rather than a filtered menu item).
    function universal_argument_map() result(map)
        type(handle_type) :: map
        type(value_type) :: key(1)
        type(status_type) :: status
        integer :: d
        character(len=4) :: name

        if (keymap_live_p(uarg_map)) then
            map = uarg_map
            return
        end if
        call make_sparse_keymap(map)
        key(1) = make_fixnum(21_int64)
        call define_key(map, key, sym("universal-argument-more"), status)
        key(1) = make_fixnum(int(iachar("-"), int64))
        call define_key(map, key, sym("universal-argument-minus"), status)
        key(1) = sym("kp-subtract")
        call define_key(map, key, sym("universal-argument-minus"), status)
        do d = 0, 9
            key(1) = make_fixnum(int(iachar("0") + d, int64))
            call define_key(map, key, sym("digit-argument"), status)
            write (name, '("kp-",i1)') d
            key(1) = sym(name)
            call define_key(map, key, sym("digit-argument"), status)
        end do
        uarg_map = map
    end function universal_argument_map

    !> simple.el:universal-argument--mode
    subroutine universal_argument_mode()
        call set_transient_map(universal_argument_map())
    end subroutine universal_argument_mode

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    subroutine cmd_newline(status)
        type(status_type), intent(out) :: status

        call newline(arg_p(), status)
    end subroutine cmd_newline

    subroutine cmd_open_line(status)
        type(status_type), intent(out) :: status

        call open_line(arg_p(), status)
    end subroutine cmd_open_line

    subroutine cmd_delete_backward_char(status)
        type(status_type), intent(out) :: status

        call delete_backward_char(arg_p(), status)
    end subroutine cmd_delete_backward_char

    !> Port of simple.el:delete-forward-char (no region).
    subroutine cmd_delete_forward_char(status)
        type(status_type), intent(out) :: status

        call delete_char(arg_p(), status)
    end subroutine cmd_delete_forward_char

    !> Port of simple.el:keyboard-quit (no mark or kbd macro handling).
    subroutine cmd_keyboard_quit(status)
        type(status_type), intent(out) :: status

        call set_error(status, FM_QUIT)
    end subroutine cmd_keyboard_quit

    !> Read the next event and insert it ARG times.  Port of
    !> simple.el:quoted-insert (without read-quoted-char's octal codes).
    subroutine cmd_quoted_insert(status)
        type(status_type), intent(out) :: status
        type(value_type) :: ev, key(1)
        logical :: have
        integer(int64) :: c

        call read_event(ev, have, status)
        if (.not. status%ok() .or. .not. have) return
        c = -1
        if (fixnump(ev)) c = xfixnum(ev)
        if (c < 0 .or. c > MAX_CHAR) then
            key(1) = ev
            call set_error(status, FM_USER_ERROR, key_description(key)// &
                           " is not a valid character")
            return
        end if
        call self_insert_command(max(arg_p(), 0_int64), c, status)
    end subroutine cmd_quoted_insert

    !> Port of simple.el:universal-argument.
    subroutine cmd_universal_argument(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call prefix_command_preserve_state()
        prefix_arg = list(make_fixnum(4_int64))
        call universal_argument_mode()
    end subroutine cmd_universal_argument

    !> Port of simple.el:universal-argument-more.
    subroutine cmd_universal_argument_more(status)
        type(status_type), intent(out) :: status
        type(value_type) :: arg

        status = status_type()
        arg = current_prefix_arg
        call prefix_command_preserve_state()
        if (consp(arg)) then
            prefix_arg = list(make_fixnum(4*xfixnum(list_elt(arg, 1_int64))))
        else if (value_eq(arg, sym("-"))) then
            prefix_arg = list(make_fixnum(-4_int64))
        else
            prefix_arg = arg
        end if
        if (consp(prefix_arg)) call universal_argument_mode()
    end subroutine cmd_universal_argument_more

    !> Port of simple.el:negative-argument.
    subroutine cmd_negative_argument(status)
        type(status_type), intent(out) :: status
        type(value_type) :: arg

        status = status_type()
        arg = current_prefix_arg
        call prefix_command_preserve_state()
        if (fixnump(arg)) then
            prefix_arg = make_fixnum(-xfixnum(arg))
        else if (value_eq(arg, sym("-"))) then
            prefix_arg = nil()
        else
            prefix_arg = sym("-")
        end if
        call universal_argument_mode()
    end subroutine cmd_negative_argument

    !> Port of simple.el:digit-argument (character events; keypad digits
    !> need the `ascii-character' property, not ported).
    subroutine cmd_digit_argument(status)
        type(status_type), intent(out) :: status
        type(value_type) :: arg
        integer(int64) :: digit

        status = status_type()
        if (.not. fixnump(last_command_event)) return
        arg = current_prefix_arg
        call prefix_command_preserve_state()
        digit = iand(xfixnum(last_command_event), 127_int64) - iachar("0")
        if (fixnump(arg)) then
            if (xfixnum(arg) < 0) then
                prefix_arg = make_fixnum(xfixnum(arg)*10 - digit)
            else
                prefix_arg = make_fixnum(xfixnum(arg)*10 + digit)
            end if
        else if (value_eq(arg, sym("-"))) then
            ! Treat -0 as just -, so that -01 will work.
            if (digit == 0) then
                prefix_arg = sym("-")
            else
                prefix_arg = make_fixnum(-digit)
            end if
        else
            prefix_arg = make_fixnum(digit)
        end if
        call universal_argument_mode()
    end subroutine cmd_digit_argument

    !> `-` in universal-argument-map: negative-argument, unless digits
    !> were typed already (upstream's menu-item filter), in which case it
    !> inserts itself as the global binding would.
    subroutine cmd_universal_argument_minus(status)
        type(status_type), intent(out) :: status

        if (fixnump(current_prefix_arg)) then
            call self_insert_command(arg_p(), int(iachar("-"), int64), status)
        else
            call cmd_negative_argument(status)
        end if
    end subroutine cmd_universal_argument_minus

    !> Register this module's commands.
    ! ------------------------------------------------------------------
    ! Vertical motion
    ! ------------------------------------------------------------------

    !> Whether the last command was next-line or previous-line.
    logical function after_line_move()
        after_line_move = value_eq(last_command, sym("next-line")) .or. &
                          value_eq(last_command, sym("previous-line"))
    end function after_line_move

    !> Move `arg` lines down (up if negative), keeping the goal column; by
    !> screen lines unless a goal column is set or line-move-visual is
    !> off.  With `noerror`, stopping early is not an error.  Port of
    !> simple.el:line-move (no vscroll, which terminals don't have).
    subroutine line_move(arg, status, noerror)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        logical :: quiet

        quiet = .false.
        if (present(noerror)) quiet = noerror
        if (line_move_visual .and. goal_column < 0) then
            call line_move_visual_1(arg, quiet, status)
        else
            call line_move_1(arg, quiet, status)
        end if
    end subroutine line_move

    !> Port of simple.el:line-move-visual: keep the cursor's screen x.
    subroutine line_move_visual_1(arg, noerror, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, moved
        logical :: ok

        opoint = point()
        if (.not. (tgc_visual .and. after_line_move())) then
            temporary_goal_column = screen_column()
            tgc_visual = .true.
        end if
        call vertical_motion(arg, moved, int(temporary_goal_column))
        if (arg >= 0) then
            ok = moved >= arg
        else
            ok = moved <= arg
            if (ok .and. point() == opoint) then
                call vertical_motion(arg, moved)
                ok = moved == arg
            end if
        end if
        if (.not. ok .and. .not. noerror) then
            if (arg < 0) then
                call set_error(status, FM_BEGINNING_OF_BUFFER)
            else
                call set_error(status, FM_END_OF_BUFFER)
            end if
        end if
    end subroutine line_move_visual_1

    !> Port of simple.el:line-move-1: move by buffer lines to the goal
    !> column (no invisible text or selective display yet).
    subroutine line_move_1(arg, noerror, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, n, shortage, npoint, column
        logical :: done

        opoint = point()
        if (tgc_visual) then
            ! (X . HSCROLL) becomes X + HSCROLL (no hscroll here).
            tgc_visual = .false.
        end if
        if (.not. after_line_move()) temporary_goal_column = current_column()

        n = arg
        done = .false.
        if (n > 0) then
            if (n > 1) shortage = forward_line(n - 1)
            call end_of_line()
            if (forward_line(1_int64) == 0) then
                n = 0
                done = .true.
            end if
        else
            if (forward_line(n) == 0) then
                if (bolp()) then
                    n = 0
                    done = .true.
                end if
            end if
        end if
        if (.not. done .and. .not. noerror) then
            if (n < 0) then
                call set_error(status, FM_BEGINNING_OF_BUFFER)
            else
                call set_error(status, FM_END_OF_BUFFER)
            end if
        end if

        if (n > 0) then
            npoint = line_end_position()
            call goto_char(opoint)
            call goto_char(npoint)
        else if (n < 0) then
            npoint = line_beginning_position()
            call goto_char(opoint)
            call goto_char(npoint)
        else
            column = temporary_goal_column
            if (goal_column >= 0) column = goal_column
            call line_move_finish(column, opoint, arg > 0)
        end if
    end subroutine line_move_1

    !> Go to `column` on the line reached, without moving backward when
    !> moving forward, and stay in the field of `opoint`.  Port of
    !> simple.el:line-move-finish (no invisible text).
    subroutine line_move_finish(column, opoint, forward)
        integer(int64), intent(in) :: column, opoint
        logical, intent(in) :: forward
        integer(int64) :: old, new, line_beg, line_end, moved, reached
        type(value_type) :: new_field, old_field
        type(status_type) :: st
        logical :: repeat

        repeat = .true.
        do while (repeat)
            repeat = .false.
            old = point()
            line_beg = line_beginning_position()
            ! (skip-chars-forward "^\n"), ignoring fields
            line_end = pos_eol()
            if (line_move_visual) then
                call vertical_motion(0_int64, moved, int(column))
            else if (column == 0) then
                call beginning_of_line()
            else
                call move_to_column(column, reached)
            end if
            if (forward .and. point() < old) call goto_char(old)
            new = point()
            if (new > line_end) then
                call backward_char(st)
                if (forward) then
                    if (line_beg < point()) then
                        new = point()
                    else
                        new = line_end
                    end if
                else if (point() <= line_end) then
                    new = point()
                else
                    new = line_end
                end if
            end if
            call goto_char(opoint)
            call get_text_property(new, sym("field"), new_field, st)
            call get_text_property(opoint, sym("field"), old_field, st)
            if (value_eq(new_field, old_field)) then
                call goto_char(new)
            else
                call goto_char(constrain_to_field(new, opoint, .true., .true.))
            end if
            if (point() < line_beg .or. point() > line_end) repeat = .true.
        end do
    end subroutine line_move_finish

    !> Move down `arg` lines (C-n, <down>).  Port of simple.el:next-line
    !> (no next-line-add-newlines).
    subroutine next_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call line_move(arg, status)
    end subroutine next_line

    !> Move up `arg` lines (C-p, <up>).  Port of simple.el:previous-line.
    subroutine previous_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call line_move(-arg, status)
    end subroutine previous_line

    subroutine cmd_next_line(status)
        type(status_type), intent(out) :: status

        call next_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_next_line

    subroutine cmd_previous_line(status)
        type(status_type), intent(out) :: status

        call previous_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_previous_line

    ! ------------------------------------------------------------------
    ! Minibuffer history
    ! ------------------------------------------------------------------

    !> The current minibuffer history list (minibuffer-history-value).
    function history_value() result(history)
        type(value_type) :: history
        type(status_type) :: status

        history = nil()
        if (.not. allocated(minibuffer_history_variable)) return
        if (len(minibuffer_history_variable) == 0) return
        call symbol_value(sym(minibuffer_history_variable), history, status)
        if (.not. status%ok()) history = nil()
    end function history_value

    !> Put history element `nabs` in the minibuffer: 0 is the input typed
    !> before moving into the history, N > 0 the Nth previous input, -1
    !> the default ("future history").  Point stays where it was unless it
    !> was at the end or in the prompt.  Port of
    !> simple.el:goto-history-element (one default; no
    !> minibuffer-default-add-function or history-sexp flag).
    subroutine goto_history_element(nabs, status)
        integer, intent(in) :: nabs
        type(status_type), intent(out) :: status
        type(value_type) :: history, elt
        character(len=:), allocatable :: text
        integer(int64) :: goal, pend
        integer :: minimum

        minimum = 0
        if (allocated(minibuffer_default)) minimum = -1
        history = history_value()
        if (minibuffer_history_position == 0 .and. .not. allocated(minibuffer_text_before_history)) then
            minibuffer_text_before_history = minibuffer_contents()
        end if
        if (nabs < minimum) then
            if (allocated(minibuffer_default)) then
                call set_error(status, FM_USER_ERROR, "End of defaults; no next item")
            else
                call set_error(status, FM_USER_ERROR, "End of history; no default available")
            end if
            return
        end if
        if (nabs > list_length(history)) then
            call set_error(status, FM_USER_ERROR, "Beginning of history; no preceding item")
            return
        end if
        ! minibuffer-temporary-goal-position
        pend = minibuffer_prompt_end()
        goal = -1
        if (point() <= pend) then
            goal = pend
        else if (point() /= point_max()) then
            goal = point()
        end if
        call del_range(pend, buffers(cur)%zv, status)
        if (.not. status%ok()) return
        minibuffer_history_position = nabs
        if (nabs < 0) then
            text = minibuffer_default
        else if (nabs == 0) then
            text = ""
            if (allocated(minibuffer_text_before_history)) then
                call move_alloc(minibuffer_text_before_history, text)
            end if
        else
            elt = list_elt(history, int(nabs, int64))
            text = ""
            if (stringp(elt)) text = xstring(elt)
        end if
        call insert(text, status)
        if (goal >= 0) then
            call goto_char(goal)
        else
            call goto_char(point_max())
        end if
    end subroutine goto_history_element

    !> M-p: the previous history element.  Port of
    !> simple.el:previous-history-element.
    subroutine cmd_previous_history_element(status)
        type(status_type), intent(out) :: status
        integer :: n

        n = int(prefix_numeric_value(current_prefix_arg))
        if (n /= 0) call goto_history_element(minibuffer_history_position + n, status)
    end subroutine cmd_previous_history_element

    !> M-n: the next history element, then the default.  Port of
    !> simple.el:next-history-element.
    subroutine cmd_next_history_element(status)
        type(status_type), intent(out) :: status
        integer :: n

        n = int(prefix_numeric_value(current_prefix_arg))
        if (n /= 0) call goto_history_element(minibuffer_history_position - n, status)
    end subroutine cmd_next_history_element

    subroutine syms_of_simple()
        ! (<up> and <down> run previous/next-line-or-history-element
        ! upstream, which in a one-line minibuffer are these.)
        call defcommand("next-line", cmd_next_line)
        call defcommand("previous-line", cmd_previous_line)
        call defcommand("previous-history-element", cmd_previous_history_element)
        call defcommand("next-history-element", cmd_next_history_element)
        call defcommand("newline", cmd_newline)
        call defcommand("open-line", cmd_open_line)
        call defcommand("delete-backward-char", cmd_delete_backward_char)
        call defcommand("delete-forward-char", cmd_delete_forward_char)
        call defcommand("keyboard-quit", cmd_keyboard_quit)
        call defcommand("quoted-insert", cmd_quoted_insert)
        call defcommand("universal-argument", cmd_universal_argument)
        call defcommand("universal-argument-more", cmd_universal_argument_more)
        call defcommand("universal-argument-minus", cmd_universal_argument_minus)
        call defcommand("negative-argument", cmd_negative_argument)
        call defcommand("digit-argument", cmd_digit_argument)
    end subroutine syms_of_simple

end module fm_simple
