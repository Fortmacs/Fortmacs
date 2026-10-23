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
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, condition_matches, FM_GENERIC_ERROR, FM_QUIT, &
                        FM_USER_ERROR, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER, FM_MARK_INACTIVE, &
                        FM_BUFFER_READ_ONLY, FM_TEXT_READ_ONLY
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, make_fixnum, list, fixnump, consp, &
                       nilp, xfixnum, list_elt, t_value, make_string, make_list, list_elements
    use lread_fm, only: sym
    use data_fm, only: value_eq, string_setting
    use character_fm, only: MAX_CHAR
    use buffer_fm, only: barf_if_buffer_read_only, buffers, cur, transient_mark_mode, vdeactivate_mark, &
                         inhibit_read_only, auto_fill_proc, undo_entry_type, UE_BOUNDARY, UE_INSERTION, UE_DELETION, &
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
    use callint_fm, only: current_prefix_arg, prefix_arg, &
                          prefix_numeric_value, mark_even_if_inactive, commandp, call_interactively
    use syntax_fm, only: forward_word, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
                         backward_prefix_chars
    use search_fm, only: looking_at, re_search_forward, re_search_backward, replace_match, regexp_quote, &
                         SEARCH_NOERROR
    use fns_fm, only: value_equal
    use insdel_fm, only: insert_before_markers
    use editfns_fm, only: char_before, eolp, point_marker, delete_and_extract_region
    use cmds_fm, only: forward_char
    use keyboard_fm, only: last_command_event, this_command, &
                           real_this_command, last_command, &
                           real_last_command, last_prefix_arg, read_event, &
                           set_transient_map, last_message, disabled_command_hook
    implicit none(type, external)
    private

    public :: execute_extended_command, read_extended_command
    public :: fundamental_mode, special_mode, syms_of_simple_modes

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
    public :: backward_word, kill_word, backward_kill_word, mark_word

    public :: delete_indentation, delete_blank_lines, fixup_whitespace, delete_horizontal_space, &
              delete_all_space, just_one_space, cycle_spacing, transpose_chars, transpose_words, &
              transpose_lines, transpose_subr, transpose_subr_1, transpose_mover, back_to_indentation, &
              beginning_of_buffer, end_of_buffer

    !> String for filling to insert at front of new line, or nil for none.
    !> Unallocated is nil.  Port of simple.el fill-prefix.
    type(string_setting), public :: fill_prefix = string_setting(name="fill-prefix")

    !> The actions of `cycle-spacing` (the symbols of
    !> `cycle-spacing-actions`).
    integer, parameter, public :: CYCLE_JUST_ONE_SPACE = 1, CYCLE_DELETE_SPACE_AFTER = 2, &
                                  CYCLE_DELETE_SPACE_BEFORE = 3, CYCLE_DELETE_ALL_SPACE = 4, CYCLE_RESTORE = 5
    !> The argument forms of an action (ACTION ARG): none (the prefix arg
    !> of the cycle), a fixed integer, `inverted-arg` and `-`.
    integer, parameter, public :: CYCLE_ARG_NONE = 0, CYCLE_ARG_FIXED = 1, CYCLE_ARG_INVERTED = 2, &
                                  CYCLE_ARG_MINUS = 3

    !> An element of `cycle_spacing_actions`: ACTION, or (ACTION ARG).
    type, public :: cycle_spacing_action_type
        integer :: action = CYCLE_JUST_ONE_SPACE
        integer :: arg_kind = CYCLE_ARG_NONE
        integer(int64) :: arg = 0
    end type cycle_spacing_action_type

    !> List of actions cycled through by `cycle-spacing'.
    !> Supported values are:
    !> - `just-one-space'      Delete all but N (prefix arg) spaces.
    !>                         See that command's docstring for details.
    !> - `delete-space-after'  Delete spaces after point keeping only N.
    !> - `delete-space-before' Delete spaces before point keeping only N.
    !> - `delete-all-space'    Delete all spaces around point.
    !> - `restore'             Restore the original spacing.
    !>
    !> All actions make use of the prefix arg given to `cycle-spacing'
    !> in the initial invocation, i.e., `just-one-space' keeps this
    !> amount of spaces deleting surplus ones.  `just-one-space' and all
    !> other actions have the contract that a positive prefix arg (or
    !> zero) only deletes tabs and spaces whereas a negative prefix arg
    !> also deletes newlines.
    !>
    !> The `delete-space-before' and `delete-space-after' actions handle
    !> the prefix arg M-- without a number provided
    !> specially: all spaces before/after point are deleted (as if N was
    !> 0) including newlines (as if N was negative).
    !>
    !> In addition, an action may take the form (ACTION ARG) where
    !> ACTION is one of the predefined actions (except for `restore')
    !> and ARG is either
    !> - an integer with the meaning that ACTION should always use this
    !>   fixed integer instead of the actual prefix arg or
    !> - the symbol `inverted-arg' with the meaning that ACTION should
    !>   be performed with the inverted actual prefix arg.
    !> - the symbol `-' with the meaning that ACTION should include
    !>   newlines but it's up to the ACTION to decide how to interpret
    !>   it as a number, e.g., `delete-space-before' and
    !>   `delete-space-after' treat it like 0 whereas `just-one-space'
    !>   treats it like -1 as is usual.  Port of simple.el
    !>   cycle-spacing-actions (no function actions).
    type(cycle_spacing_action_type), allocatable, public :: cycle_spacing_actions(:)

    ! cycle-spacing--context: :orig-pos, :whitespace-string, :n and
    ! :last-action (an index into cycle_spacing_actions, 0 for nil).
    logical :: cycle_spacing_active = .false.
    integer(int64) :: cycle_spacing_orig_pos = 0
    character(len=:), allocatable :: cycle_spacing_whitespace
    type(value_type) :: cycle_spacing_n
    integer :: cycle_spacing_last = 0

    character(len=*), parameter :: TAB = achar(9), LF = achar(10), CR = achar(13)

    public :: do_auto_fill, default_indent_new_line, internal_auto_fill, auto_fill_mode, set_fill_column

    !> Regexp to match lines that should not be auto-filled.  Port of
    !> simple.el auto-fill-inhibit-regexp (unallocated is nil).
    type(string_setting), public :: auto_fill_inhibit_regexp = string_setting(name="auto-fill-inhibit-regexp")

    !> Non-nil means to do as if `auto-fill-mode' was disabled.  Port of
    !> simple.el inhibit-auto-fill.
    logical, public :: inhibit_auto_fill = .false.

    !> The function to use for `auto-fill-function' if Auto Fill mode is turned on.
    !> Some major modes set this.  Port of simple.el
    !> normal-auto-fill-function (null is `do-auto-fill').
    procedure(auto_fill_proc), pointer, public :: normal_auto_fill_function => null()

    interface
        !> The default value for `normal-auto-fill-function'.
        !> This is the default auto-fill function, some major modes use a different one.
        !> Returns t if it really did any work.  Port of simple.el:do-auto-fill
        !> (no comments).  (In simple_fill_fm.)
        module subroutine do_auto_fill(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine do_auto_fill

        !> Break line at point and indent.
        !> If a comment syntax is defined, call `comment-line-break-function'.
        !>
        !> The inserted newline is marked hard if variable `use-hard-newlines' is true,
        !> unless optional argument SOFT is non-nil.  Port of
        !> simple.el:default-indent-new-line (no comments or hard
        !> newlines).  (In simple_fill_fm.)
        module subroutine default_indent_new_line(status, soft)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: soft
        end subroutine default_indent_new_line

        !> The function called by `self-insert-command' to perform auto-filling.
        !> Port of simple.el:internal-auto-fill.  (In simple_fill_fm.)
        module subroutine internal_auto_fill(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine internal_auto_fill

        !> Toggle automatic line breaking (Auto Fill mode).
        !>
        !> When Auto Fill mode is enabled, inserting a space at a column
        !> beyond `current-fill-column' automatically breaks the line at a
        !> previous space.
        !>
        !> When `auto-fill-mode' is on, the `auto-fill-function' variable is
        !> non-nil.
        !>
        !> The value of `normal-auto-fill-function' specifies the function to use
        !> for `auto-fill-function' when turning Auto Fill mode on.  ARG is
        !> the raw argument of a minor mode: nil or omitted toggles when
        !> TOGGLE, else enables; a positive number enables, zero or a
        !> negative number disables.  Port of simple.el:auto-fill-mode.
        !> (In simple_fill_fm.)
        module subroutine auto_fill_mode(arg, toggle)
            implicit none(type, external)
            type(value_type), intent(in), optional :: arg
            logical, intent(in), optional :: toggle
        end subroutine auto_fill_mode

        !> Set `fill-column' to specified argument.
        !> Use C-u followed by a number to specify a column.
        !> Just C-u as argument means to use the current column.  Port of
        !> simple.el:set-fill-column.  (In simple_fill_fm.)
        module subroutine set_fill_column(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine set_fill_column

        !> Register the fill commands of simple.el.  (In simple_fill_fm.)
        module subroutine syms_of_simple_fill()
            implicit none(type, external)
        end subroutine syms_of_simple_fill

        !> Major mode not specialized for anything in particular.
        !> Other major modes are defined by comparison with this one.
        !> Port of simple.el:fundamental-mode.  (In simple_modes_fm.)
        module subroutine fundamental_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine fundamental_mode

        !> Parent major mode from which special major modes should inherit.
        !>
        !> A special major mode is intended to view specially formatted data
        !> rather than files.  These modes usually use read-only buffers.
        !> Port of simple.el:special-mode.  (In simple_modes_fm.)
        module subroutine special_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine special_mode

        !> Define `fundamental-mode` and `special-mode` (with
        !> `special-mode-map`); safe to call again.  (In simple_modes_fm.)
        module subroutine syms_of_simple_modes()
            implicit none(type, external)
        end subroutine syms_of_simple_modes
    end interface

    abstract interface
        !> A motion for `transpose_subr`: move over N objects.
        subroutine transpose_mover(n, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine transpose_mover
    end interface

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
        logical :: handled
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
        handled = .false.
        if (associated(disabled_command_hook)) call disabled_command_hook(function, handled, status)
        if (.not. handled) call call_interactively(function, status)
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
        commands%execute_extended_command%run => cmd_execute_extended_command
        commands%next_line%run => cmd_next_line
        commands%previous_line%run => cmd_previous_line
        commands%previous_history_element%run => cmd_previous_history_element
        commands%next_history_element%run => cmd_next_history_element
        commands%newline%run => cmd_newline
        commands%open_line%run => cmd_open_line
        commands%delete_backward_char%run => cmd_delete_backward_char
        commands%delete_forward_char%run => cmd_delete_forward_char
        commands%keyboard_quit%run => cmd_keyboard_quit
        commands%quoted_insert%run => cmd_quoted_insert
        commands%universal_argument%run => cmd_universal_argument
        commands%universal_argument_more%run => cmd_universal_argument_more
        commands%universal_argument_minus%run => cmd_universal_argument_minus
        commands%negative_argument%run => cmd_negative_argument
        commands%digit_argument%run => cmd_digit_argument
        commands%undo%run => cmd_undo
        commands%undo_only%run => cmd_undo_only
        commands%undo_redo%run => cmd_undo_redo
        commands%set_mark_command%run => cmd_set_mark_command
        commands%exchange_point_and_mark%run => cmd_exchange_point_and_mark
        commands%mark_whole_buffer%run => cmd_mark_whole_buffer
        commands%pop_to_mark_command%run => cmd_pop_to_mark_command
        commands%kill_region%run => cmd_kill_region
        commands%copy_region_as_kill%run => cmd_copy_region_as_kill
        commands%kill_ring_save%run => cmd_kill_ring_save
        commands%kill_line%run => cmd_kill_line
        commands%kill_whole_line%run => cmd_kill_whole_line
        commands%yank%run => cmd_yank
        commands%yank_pop%run => cmd_yank_pop
        commands%rotate_yank_pointer%run => cmd_rotate_yank_pointer
        commands%backward_word%run => cmd_backward_word
        commands%kill_word%run => cmd_kill_word
        commands%backward_kill_word%run => cmd_backward_kill_word
        commands%transpose_chars%run => cmd_transpose_chars
        commands%transpose_words%run => cmd_transpose_words
        commands%transpose_lines%run => cmd_transpose_lines
        commands%delete_horizontal_space%run => cmd_delete_horizontal_space
        commands%delete_all_space%run => cmd_delete_all_space
        commands%just_one_space%run => cmd_just_one_space
        commands%cycle_spacing%run => cmd_cycle_spacing
        commands%fixup_whitespace%run => cmd_fixup_whitespace
        commands%delete_blank_lines%run => cmd_delete_blank_lines
        commands%delete_indentation%run => cmd_delete_indentation
        ! (defalias 'join-line #'delete-indentation)
        commands%join_line%run => cmd_delete_indentation
        commands%back_to_indentation%run => cmd_back_to_indentation
        commands%beginning_of_buffer%run => cmd_beginning_of_buffer
        commands%end_of_buffer%run => cmd_end_of_buffer
        call syms_of_simple_fill()
        call syms_of_simple_modes()
        cycle_spacing_actions = [cycle_spacing_action_type(CYCLE_JUST_ONE_SPACE), &
                                 cycle_spacing_action_type(CYCLE_DELETE_ALL_SPACE), &
                                 cycle_spacing_action_type(CYCLE_RESTORE)]
        commands%mark_word%run => cmd_mark_word
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
        if (forced .or. .not. transient_mark_mode%value .or. buffers(cur)%mark_active .or. mark_even_if_inactive) then
            pos = mark_position()
        else
            call set_error(status, FM_MARK_INACTIVE)
        end if
    end subroutine mark

    !> Return t if Transient Mark mode is enabled and the mark is active.
    !> Port of simple.el:region-active-p.
    logical function region_active_p()
        region_active_p = transient_mark_mode%value .and. buffers(cur)%mark_active .and. mark_position() /= 0
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
        if (flag(activate) .or. .not. transient_mark_mode%value) call set_mark(mark_position())
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

    ! ------------------------------------------------------------------
    ! Words
    ! ------------------------------------------------------------------

    !> Move backward until encountering the beginning of a word.
    !> With argument ARG, do this that many times.
    !> If ARG is omitted or nil, move point backward one word.
    !>
    !> The word boundaries are normally determined by the buffer's
    !> syntax table and character script (according to
    !> `char-script-table').  Port of simple.el:backward-word.
    subroutine backward_word(arg)
        integer(int64), intent(in) :: arg
        call forward_word(-arg)
    end subroutine backward_word

    !> Kill characters forward until encountering the end of a word.
    !> With argument ARG, do this that many times.
    !> Port of simple.el:kill-word.
    subroutine kill_word(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: start
        start = point()
        call forward_word(arg)
        call kill_region(start, point(), status)
    end subroutine kill_word

    !> Kill characters backward until encountering the beginning of a word.
    !> With argument ARG, do this that many times.
    !> Port of simple.el:backward-kill-word.
    subroutine backward_kill_word(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call kill_word(-arg, status)
    end subroutine backward_kill_word

    !> Set mark ARG words from point or move mark one word.
    !> With ALLOW-EXTEND false, mark is set ARG words from point.
    !> With ARG given (`have_arg`) and ALLOW-EXTEND (interactively, with
    !> prefix argument), the place to which mark goes is the same place
    !> \[forward-word] would move to with the same argument; if the mark
    !> is active, it moves ARG words from its current position, otherwise
    !> it is set ARG words from point.
    !> When invoked interactively without a prefix argument and no active
    !> region, mark moves one word forward.
    !> When invoked interactively without a prefix argument, and region
    !> is active, mark moves one word away of point (i.e., forward
    !> if mark is at or after point, back if mark is before point), thus
    !> extending the region by one word.  Since the direction of region
    !> extension depends on the relative position of mark and point, you
    !> can change the direction by \[exchange-point-and-mark].
    !> Port of simple.el:mark-word.
    subroutine mark_word(arg, have_arg, allow_extend)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: have_arg, allow_extend
        integer(int64) :: opoint, n, target
        opoint = point()
        if (allow_extend .and. ((value_eq(last_command, this_command) .and. mark_position() /= 0) .or. &
                                region_active_p())) then
            if (have_arg) then
                n = arg
            else if (mark_position() < point()) then
                n = -1
            else
                n = 1
            end if
            call goto_char(mark_position())
            call forward_word(n)
            target = point()
            call goto_char(opoint)
            call set_mark(target)
        else
            call forward_word(arg)
            target = point()
            call goto_char(opoint)
            call push_mark(target, nomsg=.false., activate=.true.)
        end if
    end subroutine mark_word

    !> Move backward until encountering the beginning of a word.
    !> With argument ARG, do this that many times.
    !> If ARG is omitted or nil, move point backward one word.
    !>
    !> The word boundaries are normally determined by the buffer's
    !> syntax table and character script (according to
    !> `char-script-table').  Port of simple.el:backward-word (the command).
    subroutine cmd_backward_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call backward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_backward_word

    subroutine cmd_kill_word(status)
        type(status_type), intent(out) :: status
        call kill_word(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kill_word

    subroutine cmd_backward_kill_word(status)
        type(status_type), intent(out) :: status
        call backward_kill_word(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_backward_kill_word

    subroutine cmd_mark_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call mark_word(prefix_numeric_value(current_prefix_arg),.not. nilp(current_prefix_arg), .true.)
    end subroutine cmd_mark_word

    ! ------------------------------------------------------------------
    ! Whitespace
    ! ------------------------------------------------------------------

    !> Join this line to previous and fix up whitespace at join.
    !> If there is a fill prefix, delete it from the beginning of this
    !> line.
    !> With prefix ARG, join the current line to the following line.
    !> When BEG and END are non-nil, join all lines in the region they
    !> define.  Interactively, BEG and END are, respectively, the start
    !> and end of the region if it is active, else nil.  (The region is
    !> ignored if prefix ARG is given.)  Port of
    !> simple.el:delete-indentation.
    subroutine delete_indentation(arg, status, beg, end)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: sentinel, shortage
        integer(int32) :: c
        logical :: found, matched
        character(len=:), allocatable :: prefix
        status = status_type()
        ! Consistently deactivate mark even when no text is changed.
        vdeactivate_mark = .true.
        if (present(beg) .and. present(end) .and. .not. arg) then
            ! Region is active.  Go to END, but only if region spans
            ! multiple lines.
            sentinel = beg
            call goto_char(beg)
            if (end > line_end_position()) call goto_char(end)
        else
            ! Region is inactive.  Set a loop sentinel (subtracting 1 in
            ! order to compare less than BOB).
            if (arg) then
                sentinel = line_beginning_position(2_int64) - 1
                shortage = forward_line()
            else
                sentinel = line_beginning_position() - 1
            end if
        end if
        prefix = ""
        if (allocated(fill_prefix%value)) prefix = regexp_quote(fill_prefix%value)
        do while (line_beginning_position() > sentinel)
            shortage = forward_line(0_int64)
            call char_before(c, found)
            if (.not. found .or. c /= 10) exit
            call delete_char(-1_int64, status)
            if (.not. status%ok()) return
            ! If the appended line started with the fill prefix, delete
            ! the prefix.
            if (len(prefix) > 0) then
                matched = looking_at(prefix, status)
                if (.not. status%ok()) return
                if (matched) then
                    call replace_match("", status, .true., .true.)
                    if (.not. status%ok()) return
                end if
            end if
            call fixup_whitespace(status)
            if (.not. status%ok()) return
        end do
    end subroutine delete_indentation

    !> On blank line, delete all surrounding blank lines, leaving just one.
    !> On isolated blank line, delete that one.
    !> On nonblank line, delete any immediately following blank lines.
    !> Port of simple.el:delete-blank-lines.
    subroutine delete_blank_lines(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, from, pos, shortage
        logical :: thisblank, singleblank
        status = status_type()
        ! save-excursion
        opoint = point()
        call beginning_of_line()
        thisblank = looking_at("[ "//TAB//"]*$", status)
        ! Set singleblank if there is just one blank line here.
        singleblank = .false.
        if (thisblank) then
            if (.not. looking_at("[ "//TAB//"]*"//LF//"[ "//TAB//"]*$", status)) then
                if (bobp()) then
                    singleblank = .true.
                else
                    shortage = forward_line(-1_int64)
                    singleblank = .not. looking_at("[ "//TAB//"]*$", status)
                end if
            end if
        end if
        call goto_char(opoint)
        ! Delete preceding blank lines, and this one too if it's the only
        ! one.
        if (thisblank) then
            call beginning_of_line()
            if (singleblank) shortage = forward_line(1_int64)
            from = point()
            call re_search_backward("[^ "//TAB//LF//"]", pos, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (pos > 0) then
                shortage = forward_line(1_int64)
                pos = point()
            else
                pos = point_min()
            end if
            call delete_region(from, pos, status)
            if (.not. status%ok()) return
        end if
        ! Delete following blank lines, unless the current line is blank
        ! and there are no following blank lines.
        if (.not. (thisblank .and. singleblank)) then
            opoint = point()
            call end_of_line()
            shortage = forward_line(1_int64)
            from = point()
            call re_search_forward("[^ "//TAB//LF//"]", pos, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (pos > 0) then
                call beginning_of_line()
                pos = point()
            else
                pos = point_max()
            end if
            call delete_region(from, pos, status)
            if (.not. status%ok()) return
            call goto_char(min(opoint, point_max()))
        end if
        ! Handle the special case where point is followed by newline and
        ! eob.  Delete the line, leaving point at eob.
        if (looking_at("^[ "//TAB//"]*"//LF//"\'", status)) call delete_region(point(), point_max(), status)
    end subroutine delete_blank_lines

    !> Fixup white space between objects around point.
    !> Leave one space or none, according to the context.  Port of
    !> simple.el:fixup-whitespace.
    subroutine fixup_whitespace(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: opoint, pos
        logical :: no_space
        call point_marker(saved)
        call delete_horizontal_space(.false., status)
        if (status%ok()) then
            no_space = looking_at("^\|$\|\s)", status)
            if (.not. no_space .and. status%ok()) then
                opoint = point()
                call forward_char(status, -1_int64)
                if (status%ok()) no_space = looking_at("$\|\s(\|\s'", status)
                call goto_char(opoint)
            end if
            if (status%ok() .and. .not. no_space) then
                ! (insert ?\s) inside save-excursion: point stays before it.
                call insert(" ", status)
            end if
        end if
        call marker_position(saved, pos, st)
        call goto_char(pos)
        call free_marker(saved, st)
    end subroutine fixup_whitespace

    !> Delete all spaces and tabs around point.
    !> If BACKWARD-ONLY is non-nil (interactively, the prefix argument), delete
    !> them only before point.  Port of simple.el:delete-horizontal-space.
    subroutine delete_horizontal_space(backward_only, status)
        logical, intent(in) :: backward_only
        type(status_type), intent(out) :: status
        call delete_space_internal(" "//TAB, backward_only, status)
    end subroutine delete_horizontal_space

    !> Delete all spaces, tabs, and newlines around point.
    !> If BACKWARD-ONLY is non-nil, delete them only before point.  Port of
    !> simple.el:delete-all-space.
    subroutine delete_all_space(backward_only, status)
        logical, intent(in) :: backward_only
        type(status_type), intent(out) :: status
        call delete_space_internal(" "//TAB//CR//LF, backward_only, status)
    end subroutine delete_all_space

    !> Delete CHARS around point.
    !> If BACKWARD-ONLY is non-nil, delete them only before point.  Port of
    !> simple.el:delete-space--internal.
    subroutine delete_space_internal(chars, backward_only, status)
        character(len=*), intent(in) :: chars
        logical, intent(in) :: backward_only
        type(status_type), intent(out) :: status
        integer(int64) :: orig_pos, from, to, moved
        orig_pos = point()
        if (backward_only) then
            from = orig_pos
        else
            moved = skip_chars_forward(chars, status)
            if (.not. status%ok()) return
            from = constrain_to_field(point(), orig_pos, .true., .false.)
            call goto_char(from)
        end if
        moved = skip_chars_backward(chars, status)
        if (.not. status%ok()) return
        to = constrain_to_field(point(), orig_pos, .false., .false.)
        call goto_char(to)
        call delete_region(from, to, status)
    end subroutine delete_space_internal

    !> Delete all spaces and tabs around point, leaving one space (or N spaces).
    !> Interactively, N is the prefix numeric argument.
    !> If N is negative, delete newlines as well, leaving -N spaces.
    !> See also `cycle-spacing'.  Port of simple.el:just-one-space.
    subroutine just_one_space(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: orig_pos, num, mid, end, moved
        character(len=:), allocatable :: skip_characters
        orig_pos = point()
        if (n < 0) then
            skip_characters = " "//TAB//LF//CR
        else
            skip_characters = " "//TAB
        end if
        num = abs(n)
        moved = skip_chars_backward(skip_characters, status)
        if (.not. status%ok()) return
        call goto_char(constrain_to_field(point(), orig_pos, .false., .false.))
        num = num - skip_chars_forward(" ", status, num + point())
        mid = point()
        moved = skip_chars_forward(skip_characters, status)
        end = constrain_to_field(point(), orig_pos, .true., .false.)
        call goto_char(end)
        call delete_region(mid, end, status)
        if (.not. status%ok()) return
        if (num > 0) call insert(repeat(" ", int(num)), status)
    end subroutine just_one_space

    !> Manipulate whitespace around point in a smart way.
    !> Repeated calls perform the actions in `cycle-spacing-actions' one
    !> after the other, wrapping around after the last one.
    !>
    !> All actions are amendable using a prefix arg N.  In general, a
    !> zero or positive prefix arg allows only for deletion of tabs and
    !> spaces whereas a negative prefix arg also allows for deleting
    !> newlines.
    !>
    !> The prefix arg given at the first invocation starting a cycle is
    !> provided to all following actions, i.e.,
    !>     M-- M-SPC M-SPC M-SPC
    !> is equivalent to
    !>     M-- M-SPC M-- M-SPC M-- M-SPC.
    !>
    !> A new sequence can be started by providing a different prefix arg
    !> than provided at the initial invocation (except for 1), or by
    !> doing any other command before the next M-SPC.  N is the raw
    !> prefix argument.  Port of simple.el:cycle-spacing.
    subroutine cycle_spacing(n, status)
        type(value_type), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: orig_pos, start, end, moved, numeric_n
        type(value_type) :: actual_n
        type(cycle_spacing_action_type) :: action
        integer :: next
        logical :: include_newlines
        status = status_type()
        ! Initialize `cycle-spacing--context' if needed.
        if (.not. value_eq(last_command, this_command) .or. .not. cycle_spacing_active .or. &
            (.not. nilp(n) .and. .not. value_equal(cycle_spacing_n, n))) then
            orig_pos = point()
            moved = skip_chars_backward(" "//TAB//LF//CR, status)
            call goto_char(constrain_to_field(point(), orig_pos, .false., .false.))
            start = point()
            moved = skip_chars_forward(" "//TAB//LF//CR, status)
            end = constrain_to_field(point(), orig_pos, .true., .false.)
            call buffer_substring(start, end, cycle_spacing_whitespace, status)
            call goto_char(orig_pos)
            if (.not. status%ok()) return
            cycle_spacing_orig_pos = orig_pos
            cycle_spacing_n = n
            cycle_spacing_last = 0
            cycle_spacing_active = .true.
        end if
        if (.not. allocated(cycle_spacing_actions)) return
        if (size(cycle_spacing_actions) == 0) return
        ! Cycle through the actions in `cycle-spacing-actions'.
        next = cycle_spacing_last + 1
        if (next > size(cycle_spacing_actions)) next = 1
        action = cycle_spacing_actions(next)
        ! restore
        call delete_all_space(.false., status)
        if (.not. status%ok()) return
        call insert(cycle_spacing_whitespace, status)
        if (.not. status%ok()) return
        call goto_char(cycle_spacing_orig_pos)
        if (action%action /= CYCLE_RESTORE) then
            ! The action may have a fixed numeric arg, the inverted
            ! prefix arg or `-'.
            select case (action%arg_kind)
            case (CYCLE_ARG_FIXED)
                actual_n = make_fixnum(action%arg)
            case (CYCLE_ARG_INVERTED)
                actual_n = make_fixnum(-prefix_numeric_value(cycle_spacing_n))
            case (CYCLE_ARG_MINUS)
                actual_n = sym("-")
            case default
                actual_n = cycle_spacing_n
            end select
            numeric_n = prefix_numeric_value(actual_n)
            include_newlines = value_eq(actual_n, sym("-"))
            if (fixnump(actual_n)) include_newlines = xfixnum(actual_n) < 0
            select case (action%action)
            case (CYCLE_JUST_ONE_SPACE)
                call just_one_space(numeric_n, status)
            case (CYCLE_DELETE_SPACE_AFTER)
                call delete_space(merge(0_int64, abs(numeric_n), value_eq(actual_n, sym("-"))), &
                                  include_newlines, .true.)
            case (CYCLE_DELETE_SPACE_BEFORE)
                call delete_space(merge(0_int64, abs(numeric_n), value_eq(actual_n, sym("-"))), &
                                  include_newlines, .false.)
            case (CYCLE_DELETE_ALL_SPACE)
                if (include_newlines) then
                    call delete_all_space(.false., status)
                else
                    call delete_horizontal_space(.false., status)
                end if
            case default
                call set_error(status, FM_GENERIC_ERROR, "Don't know how to handle action")
            end select
            if (.not. status%ok()) return
        end if
        cycle_spacing_last = next
    contains
        ! Delete the spaces after (FORWARD) or before point beyond the
        ! first N (all with N 0).
        subroutine delete_space(n, include_newlines, forward)
            integer(int64), intent(in) :: n
            logical, intent(in) :: include_newlines, forward
            integer(int64) :: orig_point, from, to, skipped
            character(len=:), allocatable :: chars
            orig_point = point()
            chars = " "//TAB
            if (include_newlines) chars = " "//TAB//CR//LF
            if (n /= 0) then
                if (forward) then
                    skipped = skip_chars_forward(chars, status, point() + n)
                else
                    skipped = skip_chars_backward(chars, status, point() - n)
                end if
                if (n /= abs(skipped)) return
            end if
            from = point()
            if (forward) then
                skipped = skip_chars_forward(chars, status)
            else
                skipped = skip_chars_backward(chars, status)
            end if
            to = point()
            if (from /= to) call delete_region(from, to, status)
            if (forward) then
                call goto_char(orig_point)
            else
                call goto_char(n + to)
            end if
        end subroutine delete_space
    end subroutine cycle_spacing

    ! ------------------------------------------------------------------
    ! Transposition
    ! ------------------------------------------------------------------

    !> Interchange characters around point, moving forward one character.
    !> With prefix arg ARG, effect is to take character before point
    !> and drag it forward past ARG other characters (backward if ARG negative).
    !> If at end of line, the previous two chars are exchanged.  Port of
    !> simple.el:transpose-chars.
    subroutine transpose_chars(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(value_type) :: read_only
        type(status_type) :: st
        if (eolp() .and. .not. bobp()) then
            call get_text_property(point() - 1, sym("read-only"), read_only, st)
            if (nilp(read_only)) call goto_char(point() - 1)
        end if
        call transpose_subr(move_chars, arg, status)
    end subroutine transpose_chars

    !> Interchange words around point, leaving point at end of them.
    !> With prefix arg ARG, effect is to take word before or around point
    !> and drag it forward past ARG other words (backward if ARG negative).
    !> If ARG is zero, the words around or after point and around or after mark
    !> are interchanged.  Port of simple.el:transpose-words.
    subroutine transpose_words(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call transpose_subr(move_words, arg, status)
    end subroutine transpose_words

    !> Exchange current line and previous line, leaving point after both.
    !> With argument ARG, takes previous line and moves it past ARG lines.
    !> With argument 0, interchanges line point is in with line mark is in.
    !> Port of simple.el:transpose-lines.
    subroutine transpose_lines(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call transpose_subr(move_lines, arg, status)
    end subroutine transpose_lines

    subroutine move_chars(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        call forward_char(status, n)
    end subroutine move_chars

    subroutine move_words(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        status = status_type()
        call forward_word(n)
    end subroutine move_words

    ! The mover of transpose-lines.
    subroutine move_lines(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: arg
        integer(int32) :: c
        logical :: found
        status = status_type()
        if (n > 0) then
            ! Move forward over ARG lines, but create newlines if
            ! necessary.
            arg = forward_line(n)
            call char_before(c, found)
            if (.not. found .or. c /= 10) arg = arg + 1
            if (arg > 0) call newline(arg, status)
        else
            arg = forward_line(n)
        end if
    end subroutine move_lines

    !> Subroutine to do the work of transposing objects.
    !> Works for lines, sentences, paragraphs, etc.  MOVER is a function
    !> that moves forward by units of the given
    !> object (e.g. `forward-sentence', `forward-paragraph').
    !>
    !>   If ARG is zero, exchanges the current object with the one
    !> containing mark.  If ARG is an integer, moves the current object
    !> past ARG following (if ARG is positive) or preceding (if ARG is
    !> negative) objects, leaving point after the current object.  Port of
    !> simple.el:transpose-subr (no SPECIAL movers).
    recursive subroutine transpose_subr(mover, arg, status)
        procedure(transpose_mover) :: mover
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: pos1(2), pos2(2), m, opoint
        type(handle_type) :: saved
        type(status_type) :: st
        if (arg == 0) then
            ! save-excursion
            call point_marker(saved)
            call aux(1_int64, pos1)
            if (status%ok()) then
                m = mark_position()
                if (m == 0) then
                    call set_error(status, FM_GENERIC_ERROR, "No mark set in this buffer")
                else
                    call goto_char(m)
                    call aux(1_int64, pos2)
                    if (status%ok()) call transpose_subr_1(pos1, pos2, status)
                end if
            end if
            call marker_position(saved, opoint, st)
            call goto_char(opoint)
            call free_marker(saved, st)
            if (status%ok()) call exchange_point_and_mark(.false., status)
        else if (arg > 0) then
            call aux(-1_int64, pos1)
            if (.not. status%ok()) return
            call aux(arg, pos2)
            if (.not. status%ok()) return
            call transpose_subr_1(pos1, pos2, status)
            if (.not. status%ok()) return
            call goto_char(pos2(1))
        else
            call aux(-1_int64, pos1)
            if (.not. status%ok()) return
            call goto_char(pos1(1))
            call aux(arg, pos2)
            if (.not. status%ok()) return
            call transpose_subr_1(pos1, pos2, status)
            if (.not. status%ok()) return
            call goto_char(pos2(1) + (pos1(2) - pos1(1)))
        end if
    contains
        ! (cons (progn (funcall mover x) (point))
        !       (progn (funcall mover (- x)) (point)))
        subroutine aux(x, pos)
            integer(int64), intent(in) :: x
            integer(int64), intent(out) :: pos(2)
            pos = 0
            call mover(x, status)
            if (.not. status%ok()) return
            pos(1) = point()
            call mover(-x, status)
            if (.not. status%ok()) return
            pos(2) = point()
        end subroutine aux
    end subroutine transpose_subr

    !> Exchange the text of the ranges POS1 and POS2 (each a pair of
    !> positions in either order), preserving markers at their ends.
    !> Port of simple.el:transpose-subr-1.
    subroutine transpose_subr_1(pos1_in, pos2_in, status)
        integer(int64), intent(in) :: pos1_in(2), pos2_in(2)
        type(status_type), intent(out) :: status
        integer(int64) :: pos1(2), pos2(2), swap(2), len1, len2, boundary_pos
        character(len=:), allocatable :: word
        type(handle_type) :: boundary
        type(status_type) :: st
        pos1 = [minval(pos1_in), maxval(pos1_in)]
        pos2 = [minval(pos2_in), maxval(pos2_in)]
        if (pos1(1) > pos2(1)) then
            swap = pos1
            pos1 = pos2
            pos2 = swap
        end if
        if (pos1(2) > pos2(1)) then
            call set_error(status, FM_GENERIC_ERROR, "Don't have two things to transpose")
            return
        end if
        ! This sequence of insertions attempts to preserve marker
        ! positions at the start and end of the transposed objects.
        call buffer_substring(pos2(1), pos2(2), word, status)
        if (.not. status%ok()) return
        len1 = pos1(2) - pos1(1)
        len2 = pos2(2) - pos2(1)
        call copy_marker_at(pos2(1), boundary, .false., st)
        call goto_char(pos1(2))
        call insert_before_markers(word, status)
        if (status%ok()) call delete_and_extract_region(pos1(1), pos1(1) + len1, word, status)
        if (status%ok()) then
            call marker_position(boundary, boundary_pos, st)
            call goto_char(boundary_pos)
            call insert(word, status)
        end if
        if (status%ok()) then
            call goto_char(boundary_pos + len1)
            call delete_region(point(), point() + len2, status)
        end if
        call free_marker(boundary, st)
    end subroutine transpose_subr_1

    ! The interactive whitespace and transposition commands ("*p", "*P"
    ! and "*" read the prefix argument after checking the buffer is
    ! writable).

    subroutine cmd_transpose_chars(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call transpose_chars(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_transpose_chars

    subroutine cmd_transpose_words(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call transpose_words(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_transpose_words

    subroutine cmd_transpose_lines(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call transpose_lines(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_transpose_lines

    subroutine cmd_delete_horizontal_space(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call delete_horizontal_space(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_delete_horizontal_space

    subroutine cmd_delete_all_space(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call delete_all_space(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_delete_all_space

    subroutine cmd_just_one_space(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call just_one_space(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_just_one_space

    subroutine cmd_cycle_spacing(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call cycle_spacing(current_prefix_arg, status)
    end subroutine cmd_cycle_spacing

    subroutine cmd_fixup_whitespace(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call fixup_whitespace(status)
    end subroutine cmd_fixup_whitespace

    subroutine cmd_delete_blank_lines(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call delete_blank_lines(status)
    end subroutine cmd_delete_blank_lines

    subroutine cmd_delete_indentation(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, status)
            call region_end(e, status)
            call delete_indentation(.not. nilp(current_prefix_arg), status, b, e)
        else
            call delete_indentation(.not. nilp(current_prefix_arg), status)
        end if
    end subroutine cmd_delete_indentation

    !> Move point to the first non-whitespace character on this line.
    !> Port of simple.el:back-to-indentation.
    subroutine back_to_indentation()
        integer(int64) :: moved
        call beginning_of_line(1_int64)
        moved = skip_syntax_forward(" ", line_end_position())
        ! Move back over chars that have whitespace syntax but have the p
        ! flag.
        call backward_prefix_chars()
    end subroutine back_to_indentation

    subroutine cmd_back_to_indentation(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call back_to_indentation()
    end subroutine cmd_back_to_indentation

    !> Move point to the beginning of the buffer.
    !> With numeric arg N, put point N/10 of the way from the beginning.
    !> If the buffer is narrowed, this command uses the beginning of the
    !> accessible part of the buffer.
    !>
    !> Push mark at previous position, unless either a C-u prefix
    !> is supplied, or Transient Mark mode is enabled and the mark is
    !> active.  ARG is the raw prefix argument.  Port of
    !> simple.el:beginning-of-buffer.
    subroutine beginning_of_buffer(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: size
        if (.not. consp(arg) .and. .not. region_active_p()) call push_mark()
        size = point_max() - point_min()
        if (.not. nilp(arg) .and. .not. consp(arg)) then
            call goto_char(point_min() + 1 + (size*prefix_numeric_value(arg))/10)
        else
            call goto_char(point_min())
        end if
        ! If we went to a place in the middle of the buffer, adjust it to
        ! the beginning of the following line.
        if (.not. nilp(arg) .and. .not. consp(arg)) then
            if (forward_line(1_int64) /= 0) continue
        end if
    end subroutine beginning_of_buffer

    !> Move point to the end of the buffer.
    !> With numeric arg N, put point N/10 of the way from the end.
    !> If the buffer is narrowed, this command uses the end of the
    !> accessible part of the buffer.
    !>
    !> Push mark at previous position, unless either a C-u prefix
    !> is supplied, or Transient Mark mode is enabled and the mark is
    !> active.  ARG is the raw prefix argument.  Port of
    !> simple.el:end-of-buffer (redisplay scrolls the end into view; no
    !> special recentering).
    subroutine end_of_buffer(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: size
        if (.not. consp(arg) .and. .not. region_active_p()) call push_mark()
        size = point_max() - point_min()
        if (.not. nilp(arg) .and. .not. consp(arg)) then
            call goto_char(point_max() - (size*prefix_numeric_value(arg))/10)
            ! If we went to a place in the middle of the buffer, adjust it
            ! to the beginning of a line.
            if (forward_line(1_int64) /= 0) continue
        else
            call goto_char(point_max())
        end if
    end subroutine end_of_buffer

    subroutine cmd_beginning_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call beginning_of_buffer(current_prefix_arg)
    end subroutine cmd_beginning_of_buffer

    subroutine cmd_end_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call end_of_buffer(current_prefix_arg)
    end subroutine cmd_end_of_buffer
end module simple_fm
