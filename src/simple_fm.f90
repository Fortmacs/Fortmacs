! simple_fm.f90 -- basic editing commands.
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
!> Undo (`undo`, `undo-only`, `undo-redo`, `primitive-undo`) is here; the
!> recording is undo_fm's.  The mark and region (Transient Mark mode) and the
!> kill ring are here too.
!>
!> Not ported yet: undo in a region, the global mark ring, shift-selection, the
!> interprogram (system clipboard) cut and paste, `yank-from-kill-ring`,
!> electric indentation, auto-fill and hard newlines, prefix-argument
!> echoing, octal input in quoted-insert.
module simple_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, condition_matches, FM_GENERIC_ERROR, FM_QUIT, &
                        FM_USER_ERROR, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER, FM_MARK_INACTIVE, &
                        FM_BUFFER_READ_ONLY, FM_TEXT_READ_ONLY
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, make_fixnum, list, fixnump, consp, &
                       nilp, xfixnum, list_elt, t_value, make_string, make_list, list_elements
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use character_fm, only: MAX_CHAR
    use buffer_fm, only: barf_if_buffer_read_only, buffers, cur, transient_mark_mode, vdeactivate_mark, &
                         inhibit_read_only, undo_entry_type, UE_BOUNDARY, UE_INSERTION, UE_DELETION, &
                         UE_POINT, UE_FIRST_CHANGE
    use undo_fm, only: undo_boundary
    use marker_fm, only: set_marker, detach_marker, copy_marker_at, free_marker, marker_position
    use xdisp_fm, only: message
    use subr_fm, only: add_to_history
    use editfns_fm, only: point, goto_char, point_max, line_beginning_position, line_end_position, &
                          bolp, constrain_to_field, pos_eol, point_min, bobp, eobp, buffer_substring, &
                          delete_region, mark_marker, mark_position, region_beginning, region_end
    use textprop_fm, only: get_text_property
    use indent_fm, only: current_column, move_to_column, vertical_motion, screen_column
    use insdel_fm, only: insert, del_range
    use data_fm, only: symbol_value, set
    use lisp_fm, only: list_length, xstring, stringp
    use minibuf_fm, only: minibuffer_history_position, minibuffer_history_variable, &
                          minibuffer_default, minibuffer_text_before_history, &
                          minibuffer_prompt_end, minibuffer_contents, minibuf_level
    use cmds_fm, only: delete_char, self_insert_command, forward_line, end_of_line, beginning_of_line, &
                       backward_char
    use keymap_fm, only: make_sparse_keymap, define_key, keymap_live_p, &
                         key_description, where_is_internal
    use minibuf_fm, only: completion_obarray_table
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use lread_fm, only: intern_soft
    use callint_fm, only: defcommand, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value, mark_even_if_inactive, commandp, call_interactively
    use keyboard_fm, only: last_command_event, this_command, &
                           real_this_command, last_command, &
                           real_last_command, last_prefix_arg, read_event, &
                           set_transient_map, last_message
    implicit none(type, external)
    private

    public :: execute_extended_command, read_extended_command

    !> Non-nil means show the equivalent keybinding when M-x has one.
    !> (A length of time is not supported: the suggestion shows until the
    !> next command.)  Port of simple.el suggest-key-bindings.
    logical, public :: suggest_key_bindings = .true.

    public :: newline, open_line, delete_backward_char, &
              prefix_command_preserve_state, universal_argument_map, &
              syms_of_simple, goto_history_element, line_move, next_line, previous_line
    public :: undo, undo_only, undo_redo, undo_more, undo_start, primitive_undo

    !> Within a run of consecutive undo commands, list remaining to be
    !> undone (a list position); -1 (t) if we undid all the way to the end
    !> of it.  Port of simple.el pending-undo-list.
    integer(int64), public :: pending_undo_list = 0
    !> If t, `undo` doesn't go through redo entries.  Port of simple.el
    !> undo-no-redo.
    logical, public :: undo_no_redo = .false.
    !> Non-nil while performing an undo.  Port of simple.el
    !> undo-in-progress.
    logical, public :: undo_in_progress = .false.
    ! Values of undo-equiv-table besides list positions: nil, t, `empty`.
    integer(int64), parameter :: EQUIV_NONE = -2, EQUIV_T = -1, EQUIV_EMPTY = -4
    public :: mark, region_active_p, use_region_p, deactivate_mark, activate_mark, set_mark, &
              push_mark, pop_mark, pop_to_mark_command, push_mark_command, set_mark_command, &
              exchange_point_and_mark, mark_whole_buffer
    public :: kill_ring, kill_new, kill_append, current_kill, kill_region, copy_region_as_kill, &
              kill_line, kill_whole_line, yank, yank_pop, rotate_yank_pointer, insert_for_yank, &
              forward_visible_line

    !> Maximum length of kill ring before oldest elements are thrown away.
    !> Port of simple.el kill-ring-max.
    integer(int64), public :: kill_ring_max = 120
    !> The tail of the kill ring whose car is the last thing yanked, as an
    !> index into `kill-ring` (1 for its first element).  Port of simple.el
    !> kill-ring-yank-pointer.
    integer(int64), public :: kill_ring_yank_pointer = 1
    !> Maximum size of mark ring.  Start discarding off end if gets this
    !> big.  Port of simple.el mark-ring-max.
    integer, public :: mark_ring_max = 16
    !> Non-nil means don't signal an error for killing read-only text.
    !> Port of simple.el kill-read-only-ok.
    logical, public :: kill_read_only_ok = .false.
    !> If true, `kill_line` with no arg at start of line kills the whole
    !> line.  Port of simple.el kill-whole-line (the variable).
    logical, public :: kill_whole_line_option = .false.
    !> Whether single-char deletion commands delete an active region.
    !> This has an effect only if Transient Mark mode is enabled, and
    !> affects `delete-forward-char` and `delete-backward-char`.  Port of
    !> simple.el delete-active-region (t; `kill` not yet).
    logical, public :: delete_active_region = .true.

    !> Semipermanent goal column for vertical motion, or -1 for none (nil
    !> upstream).  A setting other than -1 overrides the variable
    !> `line_move_visual`, which see.  Port of simple.el goal-column.
    integer(int64), public :: goal_column = -1
    !> When true, `line_move` moves point by visual lines.
    !> This movement is based on where the cursor is displayed on the
    !> screen, instead of relying on buffer contents alone.  It takes
    !> into account variable-width characters and line continuation.
    !> If false, `line_move` moves point by logical lines.
    !> A setting of `goal_column` overrides the value of this variable
    !> and forces movement by logical lines.
    !> A window that is horizontally scrolled also forces movement by logical
    !> lines.  Port of simple.el line-move-visual.
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

    !> Insert a newline, and move to left margin of the new line.
    !> With prefix argument ARG, insert that many newlines.
    !> Port of simple.el:newline (the command; electric indentation, auto-fill
    !> and hard newlines are not ported).
    subroutine cmd_newline(status)
        type(status_type), intent(out) :: status
        call newline(arg_p(), status)
    end subroutine cmd_newline

    !> Insert a newline and leave point before it.
    !> With arg N, insert N newlines.  Port of simple.el:open-line (no fill
    !> prefix or left margin).
    subroutine cmd_open_line(status)
        type(status_type), intent(out) :: status
        call open_line(arg_p(), status)
    end subroutine cmd_open_line

    !> Delete the previous N characters (following if N is negative).
    !> If Transient Mark mode is enabled, the mark is active, and N is 1,
    !> delete the text in the region and deactivate the mark instead.
    !> To disable this, set option `delete-active-region' to nil.
    !>
    !> A numeric prefix argument specifies N, and if a prefix argument is
    !> used, the characters are killed (saved in the kill ring) instead of
    !> deleted.  Port of simple.el:delete-backward-char (an active region is
    !> deleted; with a prefix argument the characters are killed, as KILLFLAG).
    subroutine cmd_delete_backward_char(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n
        n = arg_p()
        if (use_region_p() .and. delete_active_region .and. n == 1) then
            call delete_the_region(status)
        else if (.not. nilp(current_prefix_arg)) then
            ! cmds.c:Fdelete_char's KILLFLAG: simple.el:kill-backward-chars.
            call kill_region(point(), point() - n, status)
        else
            call delete_backward_char(n, status)
        end if
    end subroutine cmd_delete_backward_char

    !> The default region-extract-function with METHOD `delete-only`.
    subroutine delete_the_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (status%ok()) call region_end(e, status)
        if (status%ok()) call delete_region(b, e, status)
    end subroutine delete_the_region

    !> Delete the following N characters (previous if N is negative).
    !> If Transient Mark mode is enabled, the mark is active, and N is 1,
    !> delete the text in the region and deactivate the mark instead.
    !> To disable this, set variable `delete-active-region' to nil.
    !>
    !> A numeric prefix argument specifies N, and if a prefix argument is
    !> used, the characters are killed (saved in the kill ring) instead of
    !> deleted.  Port of simple.el:delete-forward-char (an active region is
    !> deleted; with a prefix argument the characters are killed, as KILLFLAG;
    !> no grapheme clusters).
    subroutine cmd_delete_forward_char(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n
        n = arg_p()
        if (use_region_p() .and. delete_active_region .and. n == 1) then
            call delete_the_region(status)
        else if (.not. nilp(current_prefix_arg)) then
            ! simple.el:kill-forward-chars.
            call kill_region(point(), point() + n, status)
        else
            call delete_char(n, status)
        end if
    end subroutine cmd_delete_forward_char

    !> Signal a `quit' condition.
    !> At top-level, as an editor command, this simply beeps.
    !> Port of simple.el:keyboard-quit (no kbd macro handling).
    subroutine cmd_keyboard_quit(status)
        type(status_type), intent(out) :: status
        if (region_active_p()) call deactivate_mark()
        call set_error(status, FM_QUIT)
    end subroutine cmd_keyboard_quit

    !> Read next input character and insert it.
    !> This is useful for inserting control characters.
    !> With argument, insert ARG copies of the character.  Port of
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

    !> Begin a numeric argument for the following command.
    !> Digits or minus sign following \[universal-argument] make up the numeric argument.
    !> \[universal-argument] following the digits or minus sign ends the argument.
    !> \[universal-argument] without digits or minus sign provides 4 as argument.
    !> Repeating \[universal-argument] without digits or minus sign
    !>  multiplies the argument by 4 each time.
    !> For some commands, just \[universal-argument] by itself serves as a flag
    !> that is different in effect from any particular numeric argument.
    !> These commands include \[set-mark-command].
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

    !> Begin a negative numeric argument for the next command.
    !> \[universal-argument] following digits or minus sign ends the argument.
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

    !> Part of the numeric argument for the next command.
    !> \[universal-argument] following digits or minus sign ends the argument.
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

    !> Move cursor vertically down ARG lines.
    !> ARG defaults to 1.
    !>
    !> If there is no character in the target line exactly under the current column,
    !> the cursor is positioned after the character in that line that spans this
    !> column, or at the end of the line if it is not long enough.
    !> If there is no line in the buffer after this one, the cursor moves to
    !> the end of the buffer.
    !>
    !> If the variable `line-move-visual' is non-nil, this command moves
    !> by display lines.  Otherwise, it moves by buffer lines, without
    !> taking variable-width characters or continued lines into account.
    !>
    !> If the variable `goal-column' is set, instead of trying to move exactly
    !> vertically (or as close as possible), this command moves to that goal
    !> column (or as close as possible).  Note that setting `goal-column'
    !> overrides `line-move-visual' and causes this command to move by buffer
    !> lines rather than by display lines.  Port of simple.el:next-line (no
    !> vscroll, `next-line-add-newlines' or `set-goal-column').
    subroutine cmd_next_line(status)
        type(status_type), intent(out) :: status
        call next_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_next_line

    !> Move cursor vertically up ARG lines.
    !> ARG defaults to 1.
    !>
    !> If there is no character in the target line exactly over the current column,
    !> the cursor is positioned after the character in that line that spans this
    !> column, or at the end of the line if it is not long enough.
    !>
    !> If the variable `line-move-visual' is non-nil, this command moves
    !> by display lines.  Otherwise, it moves by buffer lines, without
    !> taking variable-width characters or continued lines into account.
    !>
    !> If the variable `goal-column' is set, instead of trying to move exactly
    !> vertically (or as close as possible), this command moves to that goal
    !> column (or as close as possible).  Note that setting `goal-column'
    !> overrides `line-move-visual' and causes this command to move by buffer
    !> lines rather than by display lines.  Port of simple.el:previous-line (no
    !> vscroll or `set-goal-column').
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

    !> Insert into the minibuffer the Nth previous element of minibuffer history.
    !> Interactively, N is the prefix numeric argument and defaults to 1.
    !> Port of simple.el:previous-history-element.
    subroutine cmd_previous_history_element(status)
        type(status_type), intent(out) :: status
        integer :: n
        n = int(prefix_numeric_value(current_prefix_arg))
        if (n /= 0) call goto_history_element(minibuffer_history_position + n, status)
    end subroutine cmd_previous_history_element

    !> Insert into the minibuffer the Nth next element of minibuffer history.
    !> Interactively, N is the prefix numeric argument and defaults to 1.
    !> The value N can go beyond the current position in the minibuffer
    !> history,  and invoke "future history."
    !> Port of simple.el:next-history-element.
    subroutine cmd_next_history_element(status)
        type(status_type), intent(out) :: status
        integer :: n
        n = int(prefix_numeric_value(current_prefix_arg))
        if (n /= 0) call goto_history_element(minibuffer_history_position - n, status)
    end subroutine cmd_next_history_element

    ! ------------------------------------------------------------------
    ! M-x
    ! ------------------------------------------------------------------

    !> Read and return name of command to invoke via
    !> `execute-extended-command'.  Port of
    !> simple.el:read-extended-command (with read-extended-command-1; no
    !> M-X cycling, affixations or default from the buffer).
    recursive subroutine read_extended_command(name, status)
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prompt
        character(len=24) :: num
        type(value_type) :: head
        prompt = ""
        if (value_eq(current_prefix_arg, sym("-"))) then
            prompt = "- "
        else if (consp(current_prefix_arg)) then
            head = list_elt(current_prefix_arg, 1_int64)
            if (fixnump(head)) then
                if (xfixnum(head) == 4) then
                    prompt = "C-u "
                else
                    write (num, "(i0)") xfixnum(head)
                    prompt = trim(num)//" "
                end if
            end if
        else if (fixnump(current_prefix_arg)) then
            write (num, "(i0)") xfixnum(current_prefix_arg)
            prompt = trim(num)//" "
        end if
        ! Read a string, completing from and restricting to the set of all
        ! defined commands.  Save the command read on the extended-command
        ! history list.
        call completing_read(prompt//"M-x ", completion_obarray_table(), name, status, commandp, &
                             require_match=REQUIRE_MATCH_T, history="extended-command-history")
    end subroutine read_extended_command

    !> Read a command name, then read the arguments and call the command.
    !> To pass a prefix argument to the command you are invoking, give a
    !> prefix argument to `execute-extended-command'.
    !>
    !> This command provides completion when reading the command name.
    !> Port of simple.el:execute-extended-command (the key binding
    !> suggestion shows at once, and only when the command left the echo
    !> area empty; no shorter M-x suggestions).
    recursive subroutine execute_extended_command(prefixarg, status, command_name)
        type(value_type), intent(in) :: prefixarg
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: command_name
        character(len=:), allocatable :: name
        type(value_type) :: function, binding
        type(value_type), allocatable :: keys(:)
        integer(int64) :: id
        if (present(command_name)) then
            name = command_name
        else
            ! For the prompt.
            current_prefix_arg = prefixarg
            call read_extended_command(name, status)
            if (.not. status%ok()) return
        end if
        id = intern_soft(name)
        function = nil()
        if (id /= 0) function = sym(name)
        if (id == 0 .or. .not. commandp(function)) then
            call set_error(status, FM_GENERIC_ERROR, "`"//name//"' is not a valid command name")
            return
        end if
        binding = nil()
        if (suggest_key_bindings) call where_is_internal(function, binding, firstonly=.true.)
        this_command = function
        ! Normally `real-this-command' should never be changed, but here we
        ! really want to pretend that M-x <cmd> RET is nothing more than a
        ! "key binding" for <cmd>.
        real_this_command = function
        ! command-execute
        current_prefix_arg = prefixarg
        prefix_arg = nil()
        call call_interactively(function, status)
        if (.not. status%ok()) return
        if (suggest_key_bindings .and. .not. nilp(binding)) then
            if (echo_area_empty()) then
                call list_elements(binding, keys)
                call message("You can run the command "//char(226)//char(128)//char(152)//name// &
                             char(226)//char(128)//char(153)//" with "//key_description(keys))
            end if
        end if
    end subroutine execute_extended_command

    !> Whether the echo area shows nothing (current-message is nil).
    logical function echo_area_empty()
        echo_area_empty = .true.
        if (allocated(last_message)) echo_area_empty = len(last_message) == 0
    end function echo_area_empty

    subroutine cmd_execute_extended_command(status)
        type(status_type), intent(out) :: status
        type(value_type) :: arg
        ! A copy: reading the command name runs other commands, which set
        ! current-prefix-arg.
        arg = current_prefix_arg
        call execute_extended_command(arg, status)
    end subroutine cmd_execute_extended_command

    subroutine syms_of_simple()
        ! (<up> and <down> run previous/next-line-or-history-element
        ! upstream, which in a one-line minibuffer are these.)
        call defcommand("execute-extended-command", cmd_execute_extended_command)
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
        call defcommand("undo", cmd_undo)
        call defcommand("undo-only", cmd_undo_only)
        call defcommand("undo-redo", cmd_undo_redo)
        call defcommand("set-mark-command", cmd_set_mark_command)
        call defcommand("exchange-point-and-mark", cmd_exchange_point_and_mark)
        call defcommand("mark-whole-buffer", cmd_mark_whole_buffer)
        call defcommand("pop-to-mark-command", cmd_pop_to_mark_command)
        call defcommand("kill-region", cmd_kill_region)
        call defcommand("copy-region-as-kill", cmd_copy_region_as_kill)
        call defcommand("kill-ring-save", cmd_kill_ring_save)
        call defcommand("kill-line", cmd_kill_line)
        call defcommand("kill-whole-line", cmd_kill_whole_line)
        call defcommand("yank", cmd_yank)
        call defcommand("yank-pop", cmd_yank_pop)
        call defcommand("rotate-yank-pointer", cmd_rotate_yank_pointer)
    end subroutine syms_of_simple

    ! ------------------------------------------------------------------
    ! Undo
    ! ------------------------------------------------------------------

    !> The undo-equiv-table value of list position `k` in the current
    !> buffer: a list position, or EQUIV_NONE, EQUIV_T or EQUIV_EMPTY.
    integer(int64) function equiv_of(k) result(v)
        integer(int64), intent(in) :: k
        v = EQUIV_NONE
        if (.not. allocated(buffers(cur)%undo_equiv)) return
        if (k < 0 .or. k > ubound(buffers(cur)%undo_equiv, 1)) return
        v = buffers(cur)%undo_equiv(k)
    end function equiv_of

    !> Map list position `k` to `v` in undo-equiv-table.
    subroutine set_equiv(k, v)
        integer(int64), intent(in) :: k, v
        integer(int64), allocatable :: grown(:)
        associate (b => buffers(cur))
            if (.not. allocated(b%undo_equiv)) then
                allocate (b%undo_equiv(0:max(k, 64_int64)))
                b%undo_equiv = EQUIV_NONE
            else if (k > ubound(b%undo_equiv, 1)) then
                allocate (grown(0:2*k))
                grown = EQUIV_NONE
                grown(0:ubound(b%undo_equiv, 1)) = b%undo_equiv
                call move_alloc(grown, b%undo_equiv)
            end if
            b%undo_equiv(k) = v
        end associate
    end subroutine set_equiv

    !> List position `k` without its leading boundaries.
    integer(int64) function sans_leading_nil(k) result(j)
        integer(int64), intent(in) :: k
        j = k
        do while (j > 0)
            if (buffers(cur)%undo_list(j)%kind /= UE_BOUNDARY) exit
            j = j - 1
        end do
    end function sans_leading_nil

    !> Whether the list at position `k` begins with a redo record: its
    !> undo-equiv-table value.  Port of simple.el:undo--last-change-was-undo-p.
    integer(int64) function last_change_was_undo(k) result(v)
        integer(int64), intent(in) :: k
        v = equiv_of(sans_leading_nil(k))
    end function last_change_was_undo

    !> Undo N records from the front of the list at position LIST.
    !> Return what remains of the list (its position).  Port of
    !> simple.el:primitive-undo (no property, apply or marker entries).
    subroutine primitive_undo(n, list, rest, status)
        integer(int64), intent(in) :: n, list
        integer(int64), intent(out) :: rest
        type(status_type), intent(out) :: status
        type(undo_entry_type) :: next
        type(value_type) :: saved
        type(status_type) :: st
        integer(int64) :: arg, apos
        rest = list
        arg = n
        ! In a writable buffer, enable undoing read-only text that is so
        ! because of text properties.
        saved = inhibit_read_only()
        call set(sym("inhibit-read-only"), t_value(), st)
        undo_records: do while (arg > 0)
            do while (rest > 0)
                ! Exit inner loop at undo boundary.
                next = buffers(cur)%undo_list(rest)
                rest = rest - 1
                if (next%kind == UE_BOUNDARY) exit
                select case (next%kind)
                case (UE_POINT)
                    call goto_char(next%beg)
                case (UE_FIRST_CHANGE)
                    ! If this records an obsolete save (not matching the
                    ! actual disk file) then don't mark unmodified.
                    if (time_equal(next)) buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
                case (UE_INSERTION)
                    if (point_min() > next%beg .or. point_max() < next%end) then
                        call outside_error(status)
                        exit undo_records
                    end if
                    ! Set point first thing, so that undoing this undo does
                    ! not send point back to where it is now.
                    call goto_char(next%beg)
                    call delete_region(next%beg, next%end, status)
                    if (.not. status%ok()) exit undo_records
                case (UE_DELETION)
                    apos = abs(next%beg)
                    if (apos < point_min() .or. apos > point_max()) then
                        call outside_error(status)
                        exit undo_records
                    end if
                    ! Insert string and adjust point.
                    call goto_char(apos)
                    call insert(next%text, status)
                    if (.not. status%ok()) exit undo_records
                    if (next%beg > 0) call goto_char(apos)
                case default
                    call set_error(status, FM_GENERIC_ERROR, "Unrecognized entry in undo list")
                    exit undo_records
                end select
            end do
            arg = arg - 1
        end do undo_records
        call set(sym("inhibit-read-only"), saved, st)
    end subroutine primitive_undo

    subroutine outside_error(status)
        type(status_type), intent(out) :: status
        call set_error(status, FM_GENERIC_ERROR, "Changes to be undone are outside visible portion of buffer")
    end subroutine outside_error

    !> Whether a (t . TIME) entry records the visited file's current
    !> modtime (time-equal-p with visited-file-modtime; 0 for none).
    logical function time_equal(entry)
        type(undo_entry_type), intent(in) :: entry
        if (.not. (entry%known .or. buffers(cur)%modtime_known)) then
            time_equal = .true.
        else
            time_equal = (entry%known .eqv. buffers(cur)%modtime_known) .and. &
                         entry%beg == buffers(cur)%modtime_sec .and. entry%end == buffers(cur)%modtime_nsec
        end if
    end function time_equal

    !> Set `pending-undo-list` to the front of the undo list.
    !> The next call to `undo_more` will undo the most recently made
    !> change.  Port of simple.el:undo-start (not in a region).
    subroutine undo_start(status)
        type(status_type), intent(out) :: status
        if (.not. buffers(cur)%undo_enabled) then
            call set_error(status, FM_USER_ERROR, "No undo information in this buffer")
            return
        end if
        pending_undo_list = buffers(cur)%undo_count
    end subroutine undo_start

    !> Undo back N undo-boundaries beyond what was already undone recently.
    !> Call `undo_start` to get ready to undo recent changes,
    !> then call `undo_more` one or more times to undo them.  Port of
    !> simple.el:undo-more.
    subroutine undo_more(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: rest
        if (pending_undo_list < 0) then
            call set_error(status, FM_USER_ERROR, "No further undo information")
            return
        end if
        undo_in_progress = .true.
        call primitive_undo(n, pending_undo_list, rest, status)
        undo_in_progress = .false.
        pending_undo_list = rest
        if (pending_undo_list == 0) pending_undo_list = -1
    end subroutine undo_more

    !> Undo some previous changes.
    !> Repeat this command to undo more changes.
    !> A numeric ARG serves as a repeat count (ARG is the raw prefix
    !> argument).  Port of simple.el:undo (no undo in region).
    subroutine undo(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: msg
        integer(int64) :: equiv, next, list
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        ! If we get an error in undo-start, the next command should not be
        ! a "consecutive undo".
        this_command = sym("undo-start")
        ! If the previous command is `undo`, don't break the undo chain.
        if (.not. (value_eq(last_command, sym("undo")) .and. &
                   (pending_undo_list < 0 .or. last_change_was_undo(buffers(cur)%undo_count) /= EQUIV_NONE))) then
            call undo_start(status)
            if (.not. status%ok()) return
            ! Get rid of initial undo boundary.
            call undo_more(1_int64, status)
            if (.not. status%ok()) return
        end if
        ! If we got this far, the next command should be a consecutive undo.
        this_command = sym("undo")
        ! Check to see whether we're hitting a redo record.
        equiv = EQUIV_NONE
        if (pending_undo_list >= 0) equiv = equiv_of(pending_undo_list)
        if (undo_no_redo .or. equiv == EQUIV_NONE) then
            msg = "Undo"
        else
            msg = "Redo"
        end if
        if (equiv >= 0 .and. undo_no_redo) then
            ! The equiv entry might point to another redo record if we have
            ! done undo-redo-undo-redo-... so skip to the very last equiv.
            do
                next = equiv_of(equiv)
                if (next == EQUIV_NONE) exit
                equiv = next
                if (equiv < 0) exit
            end do
            pending_undo_list = -1
            if (equiv >= 0) pending_undo_list = equiv
        end if
        if (fixnump(arg)) then
            call undo_more(prefix_numeric_value(arg), status)
        else
            call undo_more(1_int64, status)
        end if
        if (.not. status%ok()) return
        ! Don't specify a position in the undo record for the undo command.
        ! Instead, undoing this should move point to where the change is.
        call strip_undo_point()
        ! Record the fact that the just-generated undo records come from an
        ! undo operation--that is, they are redo records.
        list = sans_leading_nil(buffers(cur)%undo_count)
        if (list == pending_undo_list) then
            ! Prevent identity mapping (consecutive nils in the undo list).
            next = equiv_of(list)
            if (next == EQUIV_NONE) next = EQUIV_EMPTY
            call set_equiv(list, next)
        else
            call set_equiv(list, pending_undo_list)
        end if
        if (minibuf_level == 0) call message(msg)
    end subroutine undo

    !> Remove from the newest change group the point entry (and the later
    !> ones for the same position), as `undo` does.
    subroutine strip_undo_point()
        integer(int64) :: k, j, pos
        associate (b => buffers(cur))
            k = b%undo_count
            do while (k > 0)
                if (b%undo_list(k)%kind == UE_BOUNDARY) return
                if (b%undo_list(k)%kind == UE_POINT) then
                    pos = b%undo_list(k)%beg
                    j = k
                    do while (j > 0)
                        if (b%undo_list(j)%kind == UE_BOUNDARY) exit
                        if (b%undo_list(j)%kind == UE_POINT .and. b%undo_list(j)%beg == pos) then
                            b%undo_list(j:b%undo_count - 1) = b%undo_list(j + 1:b%undo_count)
                            b%undo_count = b%undo_count - 1
                        end if
                        j = j - 1
                    end do
                    return
                end if
                k = k - 1
            end do
        end associate
    end subroutine strip_undo_point

    !> Undo some previous changes.
    !> Repeat this command to undo more changes.
    !> A numeric ARG serves as a repeat count.
    !> Contrary to `undo`, this will not redo a previous undo.  Port of
    !> simple.el:undo-only.
    subroutine undo_only(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        undo_no_redo = .true.
        call undo(make_fixnum(n), status)
        undo_no_redo = .false.
    end subroutine undo_only

    !> Undo the last ARG undos, i.e., redo the last ARG changes.
    !> Port of simple.el:undo-redo.
    subroutine undo_redo(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: ul, new_ul
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        if (last_change_was_undo(buffers(cur)%undo_count) == EQUIV_NONE) then
            call set_error(status, FM_USER_ERROR, "No undone changes to redo")
            return
        end if
        ul = sans_leading_nil(buffers(cur)%undo_count)
        undo_in_progress = .true.
        call primitive_undo(arg, ul, new_ul, status)
        undo_in_progress = .false.
        if (.not. status%ok()) return
        call message("Redo")
        this_command = sym("undo")
        pending_undo_list = last_change_was_undo(new_ul)
        if (pending_undo_list == EQUIV_NONE) pending_undo_list = 0
        ! (setq buffer-undo-list new-ul): the redo's own records go too.
        buffers(cur)%undo_count = new_ul
    end subroutine undo_redo

    subroutine cmd_undo(status)
        type(status_type), intent(out) :: status
        call undo(current_prefix_arg, status)
    end subroutine cmd_undo

    subroutine cmd_undo_only(status)
        type(status_type), intent(out) :: status
        call undo_only(arg_p(), status)
    end subroutine cmd_undo_only

    subroutine cmd_undo_redo(status)
        type(status_type), intent(out) :: status
        call undo_redo(arg_p(), status)
    end subroutine cmd_undo_redo

    ! ------------------------------------------------------------------
    ! The mark and the region
    ! ------------------------------------------------------------------

    !> Return this buffer's mark value as integer, or 0 (nil) if never set.
    !>
    !> In Transient Mark mode, this function signals an error if
    !> the mark is not active.  However, if `mark_even_if_inactive` is true,
    !> or the argument FORCE is true, it disregards whether the mark
    !> is active, and returns an integer or 0 in the usual way.  Port of
    !> simple.el:mark.
    subroutine mark(pos, status, force)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: force
        logical :: forced
        pos = 0
        forced = .false.
        if (present(force)) forced = force
        if (forced .or. .not. transient_mark_mode .or. buffers(cur)%mark_active .or. mark_even_if_inactive) then
            pos = mark_position()
        else
            call set_error(status, FM_MARK_INACTIVE)
        end if
    end subroutine mark

    !> Return t if Transient Mark mode is enabled and the mark is active.
    !> Port of simple.el:region-active-p.
    logical function region_active_p()
        region_active_p = transient_mark_mode .and. buffers(cur)%mark_active .and. mark_position() /= 0
    end function region_active_p

    !> Return t if the region is active and it is appropriate to act on it:
    !> Transient Mark mode is enabled, the mark is active and the region is
    !> not empty.  Port of simple.el:use-region-p (use-empty-active-region
    !> nil).
    logical function use_region_p()
        integer(int64) :: m
        use_region_p = region_active_p()
        if (.not. use_region_p) return
        m = mark_position()
        use_region_p = m /= point()
    end function use_region_p

    !> Deactivate the mark.
    !> If Transient Mark mode is disabled, this function normally does
    !> nothing; but if FORCE is true, it deactivates the mark anyway.
    !> Port of simple.el:deactivate-mark (no hooks or selections).
    subroutine deactivate_mark(force)
        logical, intent(in), optional :: force
        logical :: forced
        forced = .false.
        if (present(force)) forced = force
        if (region_active_p() .or. forced) buffers(cur)%mark_active = .false.
    end subroutine deactivate_mark

    !> Activate the mark.  Port of simple.el:activate-mark.
    subroutine activate_mark()
        if (mark_position() == 0) return
        if (.not. region_active_p()) buffers(cur)%mark_active = .true.
    end subroutine activate_mark

    !> Set this buffer's mark to POS (0 for none) and activate it.
    !> Normally, when a new mark is set, the old one should go on the stack.
    !> This is why most applications should use `push_mark`, not `set_mark`.
    !> Port of simple.el:set-mark.
    subroutine set_mark(pos)
        integer(int64), intent(in) :: pos
        type(handle_type) :: m
        type(status_type) :: st
        call mark_marker(m)
        if (pos > 0) then
            call set_marker(m, pos, st)
            call activate_mark()
        else
            call deactivate_mark(.true.)
            buffers(cur)%mark_active = .false.
            call detach_marker(m, st)
        end if
    end subroutine set_mark

    !> Set mark at LOCATION (point, by default) and push old mark on mark
    !> ring.  Display `Mark set' unless the optional second arg NOMSG is
    !> true.  In Transient Mark mode, activate mark if optional third arg
    !> ACTIVATE is true.  Port of simple.el:push-mark (no global mark ring).
    subroutine push_mark(location, nomsg, activate)
        integer(int64), intent(in), optional :: location
        logical, intent(in), optional :: nomsg, activate
        type(handle_type), allocatable :: ring(:)
        type(handle_type) :: m, copy
        type(status_type) :: st
        integer(int64) :: old
        integer :: n
        call mark_marker(m)
        old = mark_position()
        if (old /= 0) then
            ! The old mark goes on the front of the ring, which keeps at most
            ! mark-ring-max entries.
            call copy_marker_at(old, copy, .false., st)
            if (.not. allocated(buffers(cur)%mark_ring)) allocate (buffers(cur)%mark_ring(0))
            n = size(buffers(cur)%mark_ring)
            if (n >= mark_ring_max) then
                call free_marker(buffers(cur)%mark_ring(n), st)
                n = n - 1
            end if
            allocate (ring(n + 1))
            ring(1) = copy
            ring(2:) = buffers(cur)%mark_ring(1:n)
            call move_alloc(ring, buffers(cur)%mark_ring)
        end if
        if (present(location)) then
            call set_marker(m, location, st)
        else
            call set_marker(m, point(), st)
        end if
        if (.not. flag(nomsg) .and. minibuf_level == 0) call message("Mark set")
        if (flag(activate) .or. .not. transient_mark_mode) call set_mark(mark_position())
    end subroutine push_mark

    !> Pop off mark ring into the buffer's actual mark.
    !> Does not set point.  Does nothing if mark ring is empty.  Port of
    !> simple.el:pop-mark.
    subroutine pop_mark()
        type(handle_type), allocatable :: ring(:)
        type(handle_type) :: m, copy
        type(status_type) :: st
        integer(int64) :: pos
        integer :: n
        if (allocated(buffers(cur)%mark_ring)) then
            n = size(buffers(cur)%mark_ring)
            if (n > 0) then
                ! The current mark goes to the back; the front becomes the mark.
                call mark_marker(m)
                call copy_marker_at(mark_position(), copy, .false., st)
                call marker_position(buffers(cur)%mark_ring(1), pos, st)
                if (st%ok()) call set_marker(m, pos, st)
                call free_marker(buffers(cur)%mark_ring(1), st)
                allocate (ring(n))
                ring(1:n - 1) = buffers(cur)%mark_ring(2:n)
                ring(n) = copy
                call move_alloc(ring, buffers(cur)%mark_ring)
            end if
        end if
        call deactivate_mark()
    end subroutine pop_mark

    !> Whether optional logical `x` is present and true.
    pure logical function flag(x)
        logical, intent(in), optional :: x
        flag = .false.
        if (present(x)) flag = x
    end function flag

    !> Jump to mark, and pop a new position for mark off the ring.
    !> Port of simple.el:pop-to-mark-command.
    subroutine pop_to_mark_command(status)
        type(status_type), intent(out) :: status
        integer(int64) :: m
        m = mark_position()
        if (m == 0) then
            call set_error(status, FM_USER_ERROR, "No mark set in this buffer")
            return
        end if
        if (point() == m) call message("Mark popped")
        call goto_char(m)
        call pop_mark()
    end subroutine pop_to_mark_command

    !> Set mark at where point is.
    !> If no prefix ARG and mark is already set there, just activate it.
    !> Display `Mark set' unless the optional second arg NOMSG is true.
    !> Port of simple.el:push-mark-command.
    subroutine push_mark_command(arg, nomsg)
        logical, intent(in) :: arg
        logical, intent(in), optional :: nomsg
        integer(int64) :: m
        m = mark_position()
        if (arg .or. m == 0 .or. m /= point()) then
            call push_mark(nomsg=nomsg, activate=.true.)
        else
            call activate_mark()
            if (.not. flag(nomsg)) call message("Mark activated")
        end if
    end subroutine push_mark_command

    !> Set the mark where point is, and activate it; or jump to the mark.
    !> Setting the mark also alters the region, which is the text
    !> between point and mark; this is the closest equivalent in
    !> Emacs to what some editors call the "selection".
    !>
    !> With no prefix argument, set the mark at point, and push the
    !> old mark position on local mark ring.  Repeating the command
    !> toggles the region on and off.
    !>
    !> With prefix argument (e.g., C-u C-SPC), jump to the mark, and set the
    !> mark from position popped off the local mark ring.
    !>
    !> With C-u C-u as prefix argument, unconditionally set mark where point
    !> is.  Port of simple.el:set-mark-command (set-mark-command-repeat-pop
    !> nil, no global mark ring).
    subroutine set_mark_command(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        if (consp(arg)) then
            if (prefix_numeric_value(arg) > 4) then
                call push_mark_command(.false.)
                return
            end if
        end if
        if (.not. value_eq(this_command, sym("set-mark-command"))) then
            if (.not. nilp(arg)) then
                call pop_to_mark_command(status)
            else
                call push_mark_command(.true.)
            end if
        else if (.not. nilp(arg)) then
            this_command = sym("pop-to-mark-command")
            call pop_to_mark_command(status)
        else if (value_eq(last_command, sym("set-mark-command"))) then
            if (region_active_p()) then
                call deactivate_mark()
                call message("Mark deactivated")
            else
                call activate_mark()
                call message("Mark activated")
            end if
        else
            call push_mark_command(.false.)
        end if
    end subroutine set_mark_command

    !> Put the mark where point is now, and point where the mark is now.
    !> This command works even when the mark is not active, and it
    !> reactivates the mark.  If Transient Mark mode is on, a prefix ARG
    !> deactivates the mark if it is active, and otherwise avoids
    !> reactivating it.  Port of simple.el:exchange-point-and-mark
    !> (exchange-point-and-mark-highlight-region t).
    subroutine exchange_point_and_mark(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: omark
        omark = mark_position()
        if (omark == 0) then
            call set_error(status, FM_USER_ERROR, "No mark set in this buffer")
            return
        end if
        call set_mark(point())
        call goto_char(omark)
        if (arg .neqv. .not. region_active_p()) then
            call deactivate_mark()
        else
            call activate_mark()
        end if
    end subroutine exchange_point_and_mark

    !> Put point at beginning and mark at end of buffer.
    !> Also push mark at point before pushing mark at end of buffer.
    !> If narrowing is in effect, uses only the accessible part of the
    !> buffer.  Port of simple.el:mark-whole-buffer.
    subroutine mark_whole_buffer()
        call push_mark()
        call push_mark(point_max(), .false., .true.)
        call goto_char(minibuffer_prompt_end())
    end subroutine mark_whole_buffer

    ! ------------------------------------------------------------------
    ! The kill ring
    ! ------------------------------------------------------------------

    !> The kill ring (the value of `kill-ring`), newest first.
    function kill_ring() result(ring)
        type(value_type) :: ring
        type(status_type) :: st
        call symbol_value(sym("kill-ring"), ring, st)
        if (.not. st%ok()) ring = nil()
    end function kill_ring

    !> Make STRING the latest kill in the kill ring.
    !> Set `kill_ring_yank_pointer` to point to it.
    !> REPLACE true means that STRING will replace the front of the kill
    !> ring, rather than being added to the list.  Port of simple.el:kill-new
    !> (no interprogram cut and paste yet).
    subroutine kill_new(string, replace)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: replace
        type(value_type), allocatable :: elems(:)
        type(status_type) :: st
        if (flag(replace) .and. list_length(kill_ring()) > 0) then
            call list_elements(kill_ring(), elems)
            elems(1) = make_string(string)
            call set(sym("kill-ring"), make_list(elems), st)
        else
            call add_to_history("kill-ring", string, int(kill_ring_max), keep_all=.true.)
        end if
        kill_ring_yank_pointer = 1
    end subroutine kill_new

    !> Append STRING to the end of the latest kill in the kill ring.
    !> If BEFORE_P is true, prepend STRING to the kill instead.  Port of
    !> simple.el:kill-append.
    subroutine kill_append(string, before_p)
        character(len=*), intent(in) :: string
        logical, intent(in) :: before_p
        character(len=:), allocatable :: latest
        latest = ""
        if (list_length(kill_ring()) > 0) latest = xstring(list_elt(kill_ring(), 1_int64))
        if (before_p) then
            call kill_new(string//latest, .true.)
        else
            call kill_new(latest//string, .true.)
        end if
    end subroutine kill_append

    !> Rotate the yanking point by N places, and then return that kill.
    !> If DO_NOT_MOVE is true, then don't actually move the yanking point;
    !> just return the Nth kill forward.  Port of simple.el:current-kill
    !> (no interprogram paste).
    subroutine current_kill(n, text, status, do_not_move)
        integer(int64), intent(in) :: n
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: do_not_move
        integer(int64) :: length, k
        text = ""
        length = list_length(kill_ring())
        if (length == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Kill ring is empty")
            return
        end if
        kill_ring_yank_pointer = min(max(kill_ring_yank_pointer, 1_int64), length)
        ! (nthcdr (mod (- n (length kill-ring-yank-pointer)) (length kill-ring)) kill-ring)
        k = modulo(n - (length - kill_ring_yank_pointer + 1), length) + 1
        if (.not. flag(do_not_move)) kill_ring_yank_pointer = k
        text = xstring(list_elt(kill_ring(), k))
    end subroutine current_kill

    !> Kill ("cut") text between BEG and END.
    !> This deletes the text from the buffer and saves it in the kill ring.
    !> The command `yank` can retrieve it from there.
    !>
    !> If the previous command was also a kill command,
    !> the text killed this time appends to the text killed last time
    !> to make one entry in the kill ring.
    !>
    !> If the buffer is read-only, Emacs will beep and refrain from deleting
    !> the text, but put the text in the kill ring anyway.  This means that
    !> you can use the killing commands to copy text from a read-only buffer.
    !>
    !> With REGION true, kill the region instead of BEG to END (the
    !> command always does).  Port of simple.el:kill-region (kill-region-dwim
    !> nil); READ_ONLY_OK stands for a binding of kill-read-only-ok.
    subroutine kill_region(beg, end, status, region, read_only_ok)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region, read_only_ok
        character(len=:), allocatable :: string
        integer(int64) :: s, e
        type(status_type) :: st
        s = beg
        e = end
        if (flag(region)) then
            call region_beginning(s, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
        end if
        call buffer_substring(min(s, e), max(s, e), string, status)
        if (.not. status%ok()) return
        call delete_region(min(s, e), max(s, e), status)
        if (status%ok()) then
            if (value_eq(last_command, sym("kill-region"))) then
                call kill_append(string, end < beg)
            else
                call kill_new(string)
            end if
            this_command = sym("kill-region")
            vdeactivate_mark = .true.
            return
        end if
        if (.not. (condition_matches(status%code, FM_BUFFER_READ_ONLY) .or. &
                   condition_matches(status%code, FM_TEXT_READ_ONLY))) return
        ! The text could not be deleted: copy it to the kill ring anyway.
        call copy_region_as_kill(beg, end, st, region)
        this_command = sym("kill-region")
        if (kill_read_only_ok .or. flag(read_only_ok)) then
            status = status_type()
            call message("Read only text copied to kill ring")
        end if
    end subroutine kill_region

    !> Save the region as if killed, but don't kill it.
    !> In Transient Mark mode, deactivate the mark.  Save the text between
    !> BEG and END, or with REGION true the region.  Port of
    !> simple.el:copy-region-as-kill.
    subroutine copy_region_as_kill(beg, end, status, region)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region
        character(len=:), allocatable :: string
        integer(int64) :: s, e
        s = beg
        e = end
        if (flag(region)) then
            call region_beginning(s, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
        end if
        call buffer_substring(min(s, e), max(s, e), string, status)
        if (.not. status%ok()) return
        if (value_eq(last_command, sym("kill-region"))) then
            call kill_append(string, end < beg)
        else
            call kill_new(string)
        end if
        vdeactivate_mark = .true.
    end subroutine copy_region_as_kill

    !> Move forward by ARG lines (back for a negative ARG, to the start of
    !> the line for 0), stopping at the ends of the buffer.  Port of
    !> simple.el:forward-visible-line (no invisible text yet).
    subroutine forward_visible_line(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: n
        logical :: first
        n = arg
        if (n > 0) then
            do while (n > 0)
                if (forward_line(1_int64) /= 0) return
                n = n - 1
            end do
        else
            first = .true.
            do while (first .or. n <= 0)
                if (first) then
                    call beginning_of_line()
                else if (forward_line(-1_int64) /= 0) then
                    return
                end if
                if (point() > point_min()) n = n + 1
                first = .false.
            end do
        end if
    end subroutine forward_visible_line

    !> Kill the rest of the current line; if no nonblanks there, kill thru
    !> newline.  With prefix argument ARG, kill that many lines from point.
    !> Negative arguments kill lines backward.  With zero argument, kills
    !> the text before point on the current line.  Port of simple.el:kill-line.
    !> `have_arg` false means no argument.
    subroutine kill_line(arg, have_arg, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: have_arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rest
        integer(int64) :: start, end
        start = point()
        if (have_arg) then
            call forward_visible_line(arg)
        else
            if (eobp()) then
                call set_error(status, FM_END_OF_BUFFER)
                return
            end if
            end = line_end_position()
            call buffer_substring(start, end, rest, status)
            if (verify(rest, " "//achar(9)) == 0 .or. (kill_whole_line_option .and. bolp())) then
                call forward_visible_line(1_int64)
            else
                call goto_char(end)
            end if
        end if
        call kill_region(start, point(), status)
    end subroutine kill_line

    !> Kill current line.
    !> With prefix ARG, kill that many lines starting from the current line.
    !> If ARG is negative, kill backward.  Also kill the preceding newline.
    !> If ARG is zero, kill current line but exclude the trailing newline.
    !> Port of simple.el:kill-whole-line.
    subroutine kill_whole_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(handle_type) :: regions_begin, region1_end
        integer(int64) :: here, b, e1
        type(status_type) :: st
        here = point()
        if (arg > 0 .and. eobp()) then
            call forward_visible_line(0_int64)
            b = point()
            call goto_char(here)
            if (b == point_max()) then
                call set_error(status, FM_END_OF_BUFFER)
                return
            end if
        end if
        if (arg < 0 .and. bobp()) then
            call goto_char(line_end_position())
            b = point()
            call goto_char(here)
            if (b == point_min()) then
                call set_error(status, FM_BEGINNING_OF_BUFFER)
                return
            end if
        end if
        if (.not. value_eq(last_command, sym("kill-region"))) then
            call kill_new("")
            last_command = sym("kill-region")
        end if
        call copy_marker_at(here, regions_begin, .false., st)
        if (arg == 0) then
            call forward_visible_line(0_int64)
            call copy_marker_at(point(), region1_end, .false., st)
            call goto_char(here)
            call goto_char(line_end_position())
        else if (arg < 0) then
            call goto_char(line_end_position())
            call copy_marker_at(point(), region1_end, .false., st)
            call goto_char(here)
            call forward_visible_line(arg + 1)
            if (.not. bobp()) call goto_char(point() - 1)
        else
            call forward_visible_line(0_int64)
            call copy_marker_at(point(), region1_end, .false., st)
            call goto_char(here)
            call forward_visible_line(arg)
        end if
        call marker_position(regions_begin, b, st)
        call marker_position(region1_end, e1, st)
        call kill_region(b, e1, status, read_only_ok=b /= point())
        if (status%ok()) then
            call marker_position(regions_begin, b, st)
            call kill_region(b, point(), status)
        end if
        call free_marker(regions_begin, st)
        call free_marker(region1_end, st)
    end subroutine kill_whole_line

    !> Reinsert ("paste") the last stretch of killed text.
    !> More precisely, reinsert the most recent kill, which is the stretch of
    !> text most recently killed OR yanked, as returned by `current_kill`.
    !> Put point at the end, and set mark at the beginning without
    !> activating it.  With just C-u as argument, put point at beginning, and
    !> mark at end.  With argument N, reinsert the Nth most recent kill.
    !> ARG is the raw prefix argument.  Port of simple.el:yank.
    subroutine yank(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64) :: n, m
        this_command = t_value()
        call push_mark()
        if (nilp(arg) .or. consp(arg)) then
            n = 0
        else if (value_eq(arg, sym("-"))) then
            n = -2
        else
            n = prefix_numeric_value(arg) - 1
        end if
        call current_kill(n, text, status)
        if (.not. status%ok()) return
        call insert_for_yank(text, status)
        if (.not. status%ok()) return
        if (consp(arg)) then
            ! Like exchange-point-and-mark, without activating the mark.
            m = mark_position()
            call set_marker_here()
            call goto_char(m)
        end if
        if (value_eq(this_command, t_value())) this_command = sym("yank")
    end subroutine yank

    !> Replace just-yanked stretch of killed text with a different stretch.
    !> This command is allowed only immediately after a `yank` or a
    !> `yank_pop`.  At such a time, the region contains a stretch of
    !> reinserted previously-killed text.  `yank_pop` deletes that text and
    !> inserts in its place a different stretch of killed text.
    !>
    !> With no argument, the previous kill is inserted.
    !> With argument N, insert the Nth previous kill.
    !> If N is negative, it means to use a more recent kill.
    !>
    !> The sequence of kills wraps around, so that after the oldest one
    !> comes the newest one.  Port of simple.el:yank-pop (after other
    !> commands it is an error, as before Emacs 28, rather than
    !> yank-from-kill-ring, which needs minibuffer completion).
    subroutine yank_pop(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64) :: m, here
        logical :: before
        type(value_type) :: saved
        type(status_type) :: st
        if (.not. value_eq(last_command, sym("yank"))) then
            call set_error(status, FM_USER_ERROR, "Previous command was not a yank")
            return
        end if
        this_command = sym("yank")
        m = mark_position()
        here = point()
        before = here < m
        ! (let ((inhibit-read-only t)) ...)
        saved = inhibit_read_only()
        call set(sym("inhibit-read-only"), t_value(), st)
        call delete_region(min(here, m), max(here, m), status)
        call set(sym("inhibit-read-only"), saved, st)
        if (.not. status%ok()) return
        call set_marker_here()
        call current_kill(n, text, status)
        if (.not. status%ok()) return
        call insert_for_yank(text, status)
        if (.not. status%ok()) return
        if (before) then
            m = mark_position()
            call set_marker_here()
            call goto_char(m)
        end if
    end subroutine yank_pop

    !> Set the mark marker at point (without pushing or activating).
    subroutine set_marker_here()
        type(handle_type) :: m
        type(status_type) :: st
        call mark_marker(m)
        call set_marker(m, point(), st)
    end subroutine set_marker_here

    !> Insert STRING at point for the `yank` command.  Port of
    !> subr.el:insert-for-yank (no yank handlers or excluded properties:
    !> the kill ring holds plain strings).
    subroutine insert_for_yank(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        call insert(string, status)
    end subroutine insert_for_yank

    !> Rotate the yanking point in the kill ring.
    !> With ARG, rotate that many kills forward (or backward, if negative).
    !> Port of simple.el:rotate-yank-pointer.
    subroutine rotate_yank_pointer(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        call current_kill(arg, text, status)
    end subroutine rotate_yank_pointer

    ! ------------------------------------------------------------------
    ! Mark and kill commands
    ! ------------------------------------------------------------------

    subroutine cmd_set_mark_command(status)
        type(status_type), intent(out) :: status
        call set_mark_command(current_prefix_arg, status)
    end subroutine cmd_set_mark_command

    subroutine cmd_exchange_point_and_mark(status)
        type(status_type), intent(out) :: status
        call exchange_point_and_mark(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_exchange_point_and_mark

    subroutine cmd_mark_whole_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call mark_whole_buffer()
    end subroutine cmd_mark_whole_buffer

    subroutine cmd_pop_to_mark_command(status)
        type(status_type), intent(out) :: status
        call pop_to_mark_command(status)
    end subroutine cmd_pop_to_mark_command

    !> The region's two ends, mark first, for the kill commands; an error if
    !> there is no mark.
    subroutine mark_and_point(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status
        call mark(beg, status)
        if (.not. status%ok()) return
        end = point()
        if (beg == 0) then
            call set_error(status, FM_USER_ERROR, "The mark is not set now, so there is no region")
        end if
    end subroutine mark_and_point

    subroutine cmd_kill_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        call mark_and_point(beg, end, status)
        if (status%ok()) call kill_region(beg, end, status, region=.true.)
    end subroutine cmd_kill_region

    subroutine cmd_copy_region_as_kill(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        call mark_and_point(beg, end, status)
        if (status%ok()) call copy_region_as_kill(beg, end, status, region=.true.)
    end subroutine cmd_copy_region_as_kill

    !> Save the region as if killed, but don't kill it.
    !> In Transient Mark mode, deactivate the mark.  Port of
    !> simple.el:kill-ring-save (no indicate-copied-region yet).
    subroutine cmd_kill_ring_save(status)
        type(status_type), intent(out) :: status
        call cmd_copy_region_as_kill(status)
    end subroutine cmd_kill_ring_save

    subroutine cmd_kill_line(status)
        type(status_type), intent(out) :: status
        call kill_line(prefix_numeric_value(current_prefix_arg),.not. nilp(current_prefix_arg), status)
    end subroutine cmd_kill_line

    subroutine cmd_kill_whole_line(status)
        type(status_type), intent(out) :: status
        call kill_whole_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kill_whole_line

    subroutine cmd_yank(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call yank(current_prefix_arg, status)
    end subroutine cmd_yank

    subroutine cmd_yank_pop(status)
        type(status_type), intent(out) :: status
        call yank_pop(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_yank_pop

    subroutine cmd_rotate_yank_pointer(status)
        type(status_type), intent(out) :: status
        call rotate_yank_pointer(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rotate_yank_pointer
end module simple_fm
