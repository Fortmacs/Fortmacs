! minibuf_fm.f90 -- minibuffer input.
! Ported from GNU Emacs 31.1 src/minibuf.c.
!
! Copyright (C) 1985-1986, 1993-2026 Free Software Foundation, Inc.
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

!> Reading a line of input in the minibuffer: the prompt is read-only
!> text in a field at the start of buffer " *Minibuf-N*", shown in the
!> frame's minibuffer window, which is selected while a recursive edit
!> runs; a command throws to `exit' to finish.
!>
!> The completion primitives (`try_completion', `all_completions',
!> `test_completion') are here too; the completion UI is minibuffer_fm's.
!>
!> Not yet: hash-table collections, minibuffer-only frames and other
!> frames, input methods, minibuffer-mode, text properties in the
!> minibuffer's contents.
module minibuf_fm
    use minibuf_table_fm, only: completion_table_type, completion_predicate, completion_function, &
                                completion_list_table, completion_obarray_table, completion_function_table, &
                                COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST, COMPLETE_BOUNDARIES, TABLE_LIST, &
                                TABLE_OBARRAY, TABLE_FUNCTION, TABLE_QUOTING, completion_quote_proc
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use eval_fm, only: hook_type
    use lisp_fm, only: value_type, nil, t_value, list, nilp, make_string, make_list, list_elements, make_fixnum, &
                       list_length, list_elt, fixnump, xfixnum, make_symbol_ref, stringp, xstring, symbolp, &
                       xsymbol, symbols, nsymbols, V_LIST, vtag
    use lread_fm, only: intern_soft
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring
    use search_fm, only: string_match, match_end, regexp_quote
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value, kill_all_local_variables, logical_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, &
                         erase_buffer, resolve_buffer, inhibit_read_only, buffer_live_p
    use insdel_fm, only: insert
    use editfns_fm, only: field_end, buffer_substring, goto_char
    use textprop_fm, only: add_text_properties, get_text_property
    use keymap_fm, only: make_sparse_keymap, use_local_map, keymap_live_p
    use keyboard_fm, only: recursive_edit_1, throw_exit, throw_exit_string, command_loop_level, last_message, &
                           minibuf_level, barf_if_interaction_inhibited, safe_run_hooks
    use search_fm, only: fast_string_match
    use frame_fm, only: selected_frame, frame_selected_window_ref, minibuffer_window_ref
    use window_fm, only: window_type, set_window_buffer, window_configuration_type, current_window_configuration, &
                         set_window_configuration, free_window_configuration, minibuffer_scroll_window
    use subr_fm, only: add_to_history
    implicit none(type, external)
    private

    public :: minibuf_level
    public :: minibuffer_depth, minibuffer_prompt, is_minibuffer, live_minibuffer_p, nth_minibuffer, &
              run_exit_minibuf_hook, set_initial_minibuffer_mode, match_regexps, assoc_string
    public :: read_from_minibuffer, read_string, minibuffer_prompt_end, minibuffer_contents, &
              minibufferp, abort_minibuffers, init_minibuf, minibuffer_innermost_command_loop_p, &
              innermost_minibuffer_p, minibuffer_depth_of, minibuffer_setup_proc
    public :: quoting_table_call, complete_with_action, completion_table_subvert, completion_table_with_predicate
    public :: try_completion, all_completions, test_completion, completion_list_table, &
              completion_obarray_table, completion_function_table, completion_predicate, &
              completion_function, completion_table_type, COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST, &
              COMPLETE_BOUNDARIES, TABLE_LIST, TABLE_OBARRAY, TABLE_FUNCTION

    abstract interface
        !> A function run once the minibuffer is set up (see
        !> `minibuffer_setup_function').  Not a port.
        subroutine minibuffer_setup_proc()
            implicit none(type, external)
        end subroutine minibuffer_setup_proc
    end interface

    !> True means don't consider case significant in completion.
    !> For file-name completion, `read_file_name_completion_ignore_case'
    !> controls the behavior, rather than this variable.
    !> For buffer name completion, `read_buffer_completion_ignore_case'
    !> controls the behavior, rather than this variable.  Port of minibuf.c
    !> completion-ignore-case.
    type(logical_setting), public :: completion_ignore_case = logical_setting(name="completion_ignore_case")
    !> True means to allow minibuffer commands while in the minibuffer.
    !> This variable makes a difference whenever the minibuffer window is active.
    !> Port of minibuf.c enable-recursive-minibuffers.
    logical, public :: enable_recursive_minibuffers = .false.
    !> True means restore window configurations on exit from minibuffer.
    !> If this is true (the default), reading input with the minibuffer
    !> will restore, on exit, the window configuration of the frame where
    !> the minibuffer was entered from.
    !>
    !> If this is false, window configurations are not restored upon
    !> exiting the minibuffer.  Port of minibuf.c
    !> read-minibuffer-restore-windows (`minibuffer_restore_windows' in
    !> `minibuffer_exit_hook' is deferred).
    logical, public :: read_minibuffer_restore_windows = .true.
    !> minibuffer-local-map, the default keymap for reading.
    type(handle_type), public :: minibuffer_local_map
    !> True means to add new elements in history.
    !> If set to false, minibuffer reading functions don't add new elements to the
    !> history list, so it is possible to do this afterwards by calling
    !> `add_to_history' explicitly.  Port of minibuf.c history-add-new-input.
    logical, public :: history_add_new_input = .true.

    !> Whether to hide input characters in noninteractive mode.
    !> If not -1, it must be a character, which will be used to mask the
    !> input characters.  This variable should never be set globally.
    !>
    !> This variable also overrides the default character that
    !> `read_passwd' uses to hide passwords.  Port of minibuf.c
    !> read-hide-char (-1 for nil).
    integer(int32), public :: read_hide_char = -1

    !> Normal hook run just after entry to minibuffer.  Port of minibuf.c
    !> minibuffer-setup-hook (the function of `minibuffer_with_setup_hook'
    !> runs first).
    type(hook_type), target, public :: minibuffer_setup_hook = hook_type(name="minibuffer_setup_hook")

    !> Normal hook run whenever a minibuffer is exited.  Port of minibuf.c
    !> minibuffer-exit-hook.
    type(hook_type), target, public :: minibuffer_exit_hook = hook_type(name="minibuffer_exit_hook")

    !> List of regexps that should restrict possible completions.
    !> The basic completion functions only consider a completion acceptable
    !> if it matches all regular expressions in this list, with
    !> `case_fold_search' bound to the value of `completion_ignore_case'.
    !> See Info node `(elisp)Basic Completion', for a description of these
    !> functions.
    !>
    !> Do not set this variable to a non-nil value globally, as that is not
    !> safe and will probably cause errors in completion commands.  This
    !> variable should be only let-bound to non-nil values around calls to
    !> basic completion functions like `try_completion' and `all_completions'.
    !> Port of minibuf.c completion-regexp-list (unallocated for nil).
    character(len=:), allocatable, public :: completion_regexp_list(:)

    !> minibuf.c:minibuf_prompt, the prompt of the active minibuffer
    !> (unallocated for nil).
    character(len=:), allocatable :: minibuf_prompt

    !> Vminibuffer_history_variable: the history list's symbol name.
    character(len=:), allocatable, public :: minibuffer_history_variable
    !> Vminibuffer_history_position: the element shown (0: the input).
    integer, public :: minibuffer_history_position = 0
    !> A string in a list of strings.
    type, public :: minibuffer_string_type
        character(len=:), allocatable :: s
    end type minibuffer_string_type

    !> minibuffer-default: the default values, for M-n ("future
    !> history"); unallocated (or empty) for nil.
    type(minibuffer_string_type), allocatable, public :: minibuffer_default(:)
    !> simple.el minibuffer-temporary-goal-position: where M-n and M-p
    !> leave point (0 for the end), kept while they repeat.
    integer(int64), public :: minibuffer_temporary_goal_position = 0
    !> simple.el minibuffer-text-before-history: the input before moving
    !> into the history; unallocated for nil.
    character(len=:), allocatable, public :: minibuffer_text_before_history

    !> A string in an array.
    type :: string_box
        character(len=:), allocatable :: s
    end type string_box

    !> The buffer that was current when the innermost active minibuffer
    !> was entered (the buffer of minibuffer-selected-window); the null
    !> handle when no minibuffer is active.
    type(handle_type), public, protected :: minibuffer_calling_buffer

    !> Vminibuffer_list: the minibuffer of each depth (index depth + 1).
    type(handle_type), allocatable :: minibuffers(:)
    !> Vcommand_loop_level_list: the command loop level of each depth's
    !> recursive edit (index depth + 1).
    integer, allocatable :: minibuffer_loop_levels(:)

    !> A function to run once by the next `read_from_minibuffer', after
    !> the minibuffer is current and set up (its local keymap made, the
    !> initial input inserted), as minibuffer-setup-hook would run one
    !> added by subr.el:minibuffer-with-setup-hook; null for none.  It is
    !> cleared before it runs, so recursive minibuffers don't run it.
    procedure(minibuffer_setup_proc), pointer, public :: minibuffer_setup_function => null()

    !> Whether the last `read_from_minibuffer' added its input (or
    !> default) to the history list, so that it is the list's first
    !> element (what read-file-name tests with `eq').
    logical, public, protected :: history_added = .false.

    ! The predicates `both_preds' combines (completion_table_with_predicate).
    procedure(completion_predicate), pointer :: and_pred1 => null(), and_pred2 => null()

contains

    !> Create minibuffer-local-map (its bindings are made by minibuffer.el
    !> and bindings.el, i.e. minibuffer_fm and bindings_fm).  Port of
    !> minibuf.c:init_minibuf_once (upstream makes the map in
    !> keymap.c:syms_of_keymap).
    subroutine init_minibuf()
        if (.not. keymap_live_p(minibuffer_local_map)) call make_sparse_keymap(minibuffer_local_map)
        minibuf_level = 0
    end subroutine init_minibuf

    !> The minibuffer for depth `depth', " *Minibuf-DEPTH*", made if
    !> needed.  Port of minibuf.c:get_minibuffer.
    subroutine get_minibuffer(depth, buf, status)
        integer, intent(in) :: depth
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: grown(:)
        integer, allocatable :: grown_levels(:)
        character(len=20) :: num
        integer(int64) :: i

        if (.not. allocated(minibuffers)) allocate (minibuffers(4))

        if (.not. allocated(minibuffer_loop_levels)) then
            allocate (minibuffer_loop_levels(4))
            minibuffer_loop_levels = 0
        end if

        if (depth + 1 > size(minibuffers)) then
            allocate (grown(2*(depth + 1)))
            grown(1:size(minibuffers)) = minibuffers
            call move_alloc(grown, minibuffers)
            allocate (grown_levels(2*(depth + 1)))
            grown_levels = 0
            grown_levels(1:size(minibuffer_loop_levels)) = minibuffer_loop_levels
            call move_alloc(grown_levels, minibuffer_loop_levels)
        end if

        call resolve_buffer(minibuffers(depth + 1), i, status)

        if (status%ok()) then
            buf = minibuffers(depth + 1)
            return
        end if

        write (num, "(i0)") depth
        call get_buffer_create(" *Minibuf-"//trim(num)//"*", buf, status)
        if (.not. status%ok()) return
        minibuffers(depth + 1) = buf
    end subroutine get_minibuffer

    !> Whether `buf' (default: the current buffer) is a minibuffer; with
    !> LIVE, an active one.  Port of minibuf.c:Fminibufferp.
    logical function minibufferp(buf, live)
        type(handle_type), intent(in), optional :: buf
        logical, intent(in), optional :: live
        type(handle_type) :: b
        integer :: k

        if (present(buf)) then
            b = buf
        else
            b = current_buffer()
        end if

        if (present(live)) then
            if (live) then
                minibufferp = live_minibuffer_p(b)
                return
            end if
        end if

        minibufferp = .false.
        if (.not. allocated(minibuffers)) return

        do k = 1, size(minibuffers)
            if (minibuffers(k)%index == b%index .and. minibuffers(k)%generation == b%generation .and. &
                b%index /= 0) then
                minibufferp = .true.
                return
            end if
        end do
    end function minibufferp

    !> The depth of the active minibuffer `buf' (default: the current
    !> buffer), or 0 if it is not one.  Port of
    !> minibuf.c:this_minibuffer_depth.
    integer function minibuffer_depth_of(buf) result(depth)
        type(handle_type), intent(in), optional :: buf
        type(handle_type) :: b
        integer :: k

        if (present(buf)) then
            b = buf
        else
            b = current_buffer()
        end if

        depth = 0
        if (.not. allocated(minibuffers) .or. b%index == 0) return

        do k = 2, min(minibuf_level + 1, size(minibuffers))
            if (minibuffers(k)%index == b%index .and. minibuffers(k)%generation == b%generation) then
                depth = k - 1
                return
            end if
        end do
    end function minibuffer_depth_of

    !> Return true if BUFFER is a minibuffer at the current command loop level.
    !> BUFFER defaults to the current buffer.  Port of
    !> minibuf.c:Fminibuffer_innermost_command_loop_p.
    logical function minibuffer_innermost_command_loop_p(buffer) result(innermost)
        type(handle_type), intent(in), optional :: buffer
        integer :: depth

        depth = minibuffer_depth_of(buffer)
        innermost = depth > 0
        if (innermost) innermost = minibuffer_loop_levels(depth + 1) == command_loop_level
    end function minibuffer_innermost_command_loop_p

    !> Return true if BUFFER is the most nested active minibuffer.
    !> No recursive minibuffers are active if BUFFER is the minibuffer.
    !> BUFFER defaults to the current buffer.  Port of
    !> minibuf.c:Finnermost_minibuffer_p.
    logical function innermost_minibuffer_p(buffer) result(innermost)
        type(handle_type), intent(in), optional :: buffer

        innermost = minibuffer_depth_of(buffer) == minibuf_level .and. minibuf_level > 0
    end function innermost_minibuffer_p

    !> Where the prompt ends in the current minibuffer (point-min if the
    !> current buffer is not one).  Port of minibuf.c:Fminibuffer_prompt_end.
    integer(int64) function minibuffer_prompt_end() result(pos)
        type(value_type) :: field
        type(status_type) :: status

        pos = buffers(cur)%begv
        if (.not. minibufferp()) return
        pos = field_end(buffers(cur)%begv, .false.)

        if (pos == buffers(cur)%zv) then
            call get_text_property(buffers(cur)%begv, sym("field"), field, status)
            if (nilp(field)) pos = buffers(cur)%begv
        end if
    end function minibuffer_prompt_end

    !> The text of the current minibuffer after the prompt.  Port of
    !> minibuf.c:Fminibuffer_contents_no_properties.
    function minibuffer_contents() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: status

        call buffer_substring(minibuffer_prompt_end(), buffers(cur)%zv, text, status)
    end function minibuffer_contents

    !> Read a string in the minibuffer with `prompt', starting with
    !> `initial' (point after it), using `keymap' (default
    !> minibuffer-local-map) and history list `history' (default
    !> "minibuffer-history").  C-g (abort-minibuffers) gives a quit.
    !> `default' is the default value (for M-n and the history), or
    !> `defaults' a list of them (the first is the default).  Port of
    !> minibuf.c:read_minibuf with Fread_from_minibuffer (no READ, no
    !> INHERIT-INPUT-METHOD; DEFAULT only for the history), with
    !> minibuf.c:Fread_from_minibuffer, minibuf.c:read_minibuf_unwind and
    !> minibuf.c:choose_minibuf_frame (one frame).  minibuffer-mode is not
    !> ported: as minibuf.c:set_minibuffer_mode does when it is not
    !> defined, the minibuffer's local variables are killed.
    recursive subroutine read_from_minibuffer(prompt, result, status, initial, keymap, history, default, defaults, &
                                              position, history_position)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default
        type(handle_type), intent(in), optional :: keymap
        type(minibuffer_string_type), intent(in), optional :: defaults(:)
        !> With INITIAL, point goes to this one-indexed position in it (as
        !> for an INITIAL of the form (STRING . POSITION)).
        integer(int64), intent(in), optional :: position
        !> The initial history position (as for a HIST of the form
        !> (SYMBOL . POSITION)); default 0.
        integer, intent(in), optional :: history_position
        type(handle_type) :: calling_buffer, minibuffer, old_mini_buffer, saved_calling
        type(window_configuration_type) :: config
        type(window_type), pointer :: sw, mw
        type(status_type) :: st, edit_status
        type(value_type) :: histval, saved_inhibit
        character(len=:), allocatable :: ambient_dir, saved_histvar, histvar, saved_before, histstring
        type(minibuffer_string_type), allocatable :: saved_default(:)
        character(len=:), allocatable :: saved_prompt
        logical :: had_prompt
        integer(int64) :: saved_goal
        integer :: saved_histpos
        logical :: had_default, had_before, have_histstring
        logical :: changed, old_selected, have_dir
        procedure(minibuffer_setup_proc), pointer :: setup

        result = ""
        history_added = .false.
        call barf_if_interaction_inhibited(status)
        if (.not. status%ok()) return

        if (.not. enable_recursive_minibuffers .and. minibuf_level > 0) then
            if (selected_frame%minibuffer_selected) then
                call set_error(status, FM_USER_ERROR, "Command attempted to use minibuffer while in minibuffer")
            else
                ! If we're in another window, cancel the minibuffer that's
                ! active.
                call throw_exit_string("Command attempted to use minibuffer while in minibuffer")
                call set_error(status, FM_GENERIC_ERROR, "Command attempted to use minibuffer while in minibuffer")
            end if

            return
        end if

        histvar = "minibuffer_history"
        if (present(history)) histvar = history
        calling_buffer = current_buffer()
        have_dir = allocated(buffers(cur)%directory)
        if (have_dir) ambient_dir = buffers(cur)%directory
        call get_minibuffer(minibuf_level + 1, minibuffer, status)
        if (.not. status%ok()) return
        minibuf_level = minibuf_level + 1
        ! Vcommand_loop_level_list: the level of this minibuffer's
        ! recursive edit.
        minibuffer_loop_levels(minibuf_level + 1) = command_loop_level
        saved_calling = minibuffer_calling_buffer
        minibuffer_calling_buffer = calling_buffer

        ! minibuf_save_list
        if (allocated(minibuffer_history_variable)) then
            saved_histvar = minibuffer_history_variable
        else
            saved_histvar = ""
        end if

        saved_histpos = minibuffer_history_position
        had_prompt = allocated(minibuf_prompt)
        if (had_prompt) call move_alloc(minibuf_prompt, saved_prompt)
        minibuf_prompt = prompt
        minibuffer_history_variable = histvar
        minibuffer_history_position = 0
        if (present(history_position)) minibuffer_history_position = history_position
        had_default = allocated(minibuffer_default)
        if (had_default) call move_alloc(minibuffer_default, saved_default)

        if (present(defaults)) then
            if (size(defaults) > 0) minibuffer_default = defaults
        else if (present(default)) then
            allocate (minibuffer_default(1))
            minibuffer_default(1)%s = default
        end if

        saved_goal = minibuffer_temporary_goal_position
        minibuffer_temporary_goal_position = 0
        had_before = allocated(minibuffer_text_before_history)
        if (had_before) call move_alloc(minibuffer_text_before_history, saved_before)

        ! If variable is unbound, make it nil (a HISTORY of "t" keeps no
        ! history).
        if (histvar /= "t") then
            call symbol_value(sym(histvar), histval, st)
            if (.not. st%ok()) call set(sym(histvar), nil(), st)
        end if

        old_selected = selected_frame%minibuffer_selected
        sw => frame_selected_window_ref()
        mw => minibuffer_window_ref()
        old_mini_buffer = mw%buffer
        if (read_minibuffer_restore_windows) call current_window_configuration(config)
        minibuffer_scroll_window = selected_frame%selected_window
        call set_buffer(minibuffer, status)

        if (.not. status%ok()) then
            minibuf_level = minibuf_level - 1
            minibuffer_calling_buffer = saved_calling
            return
        end if

        ! set_minibuffer_mode
        call kill_all_local_variables()
        ! The minibuffer works in the caller's directory.
        if (have_dir) buffers(cur)%directory = ambient_dir
        call set_window_buffer(selected_frame%minibuffer_window, minibuffer, st)
        selected_frame%minibuffer_selected = .true.
        mw%hscroll = 0
        ! Erase the buffer.
        ! (specbind Qinhibit_read_only Qnil), restored on exit
        saved_inhibit = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
        call erase_buffer(st)
        buffers(cur)%multibyte = .true.

        ! Insert the prompt, record where it ends.
        if (len(prompt) > 0) then
            call insert(prompt, st)
            ! front-sticky, rear-nonsticky and field, then
            ! minibuffer-prompt-properties (by default read-only t and the
            ! face minibuffer_prompt, added after any faces of the prompt).
            call add_text_properties(1_int64, buffers(cur)%pt, &
                                     list(sym("front_sticky"), t_value(), sym("rear_nonsticky"), t_value(), &
                                          sym("field"), t_value(), sym("read_only"), t_value()), changed, st)
            call add_text_properties(1_int64, buffers(cur)%pt, list(sym("face"), sym("minibuffer_prompt")), &
                                     changed, st)
        end if

        call set(sym("inhibit_read_only"), nil(), st)

        if (present(initial)) then
            call insert(initial, st)

            if (present(position)) then
                ! Convert to distance from end of input.
                if (position < 1) then
                    ! A number too small means the beginning of the string.
                    call goto_char(buffers(cur)%pt - schars(initial))
                else
                    call goto_char(buffers(cur)%pt + max(position - 1 - schars(initial), -schars(initial)))
                end if
            end if
        end if

        if (present(keymap)) then
            call use_local_map(keymap, st)
        else
            call use_local_map(minibuffer_local_map, st)
        end if

        ! The minibuffer replaces whatever the echo area showed.
        if (allocated(last_message)) last_message = ""

        ! minibuffer-setup-hook (a function `minibuffer_with_setup_hook'
        ! added for this minibuffer).
        if (associated(minibuffer_setup_function)) then
            setup => minibuffer_setup_function
            minibuffer_setup_function => null()
            call setup()
        end if

        call minibuffer_setup_hook%run(st)
        call recursive_edit_1(edit_status)
        ! Make the minibuffer contents into a string.
        call set_buffer(minibuffer, st)
        result = minibuffer_contents()
        ! Choose the string to add to the history.
        have_histstring = .true.

        if (len(result) /= 0) then
            histstring = result
        else if (present(defaults)) then
            if (size(defaults) > 0) then
                histstring = defaults(1)%s
            else
                have_histstring = .false.
            end if
        else if (present(default)) then
            histstring = default
        else
            have_histstring = .false.
        end if

        call run_exit_minibuf_hook(minibuffer)
        ! read_minibuf_unwind
        minibuf_level = minibuf_level - 1
        if (allocated(minibuf_prompt)) deallocate (minibuf_prompt)
        if (had_prompt) call move_alloc(saved_prompt, minibuf_prompt)
        call set(sym("inhibit_read_only"), t_value(), st)
        call erase_buffer(st)
        call set(sym("inhibit_read_only"), saved_inhibit, st)
        minibuffer_calling_buffer = saved_calling
        selected_frame%minibuffer_selected = old_selected
        ! (Windows may have been made meanwhile: fetch them again.)
        ! minibuf.c:minibuffer_unwind (one frame)
        mw => minibuffer_window_ref()
        mw%buffer = old_mini_buffer

        if (read_minibuffer_restore_windows) then
            call set_window_configuration(config)
            call free_window_configuration(config)
        end if

        call set_buffer(calling_buffer, st)
        minibuffer_history_variable = saved_histvar
        minibuffer_history_position = saved_histpos
        if (allocated(minibuffer_default)) deallocate (minibuffer_default)
        if (had_default) call move_alloc(saved_default, minibuffer_default)
        minibuffer_temporary_goal_position = saved_goal
        if (allocated(minibuffer_text_before_history)) deallocate (minibuffer_text_before_history)
        if (had_before) call move_alloc(saved_before, minibuffer_text_before_history)

        if (.not. edit_status%ok()) then
            status = edit_status
            result = ""
            return
        end if

        ! Add the value to the appropriate history list, if any.
        if (history_add_new_input .and. have_histstring .and. histvar /= "t") then
            call add_to_history(histvar, histstring, added=history_added)
        end if
    end subroutine read_from_minibuffer

    !> Return current depth of activations of minibuffer, a nonnegative integer.
    !> Port of minibuf.c:Fminibuffer_depth.
    integer function minibuffer_depth()
        minibuffer_depth = minibuf_level
    end function minibuffer_depth

    !> Return the prompt string of the currently-active minibuffer.
    !> If no minibuffer is active, return false in HAVE.  Port of
    !> minibuf.c:Fminibuffer_prompt.
    subroutine minibuffer_prompt(prompt, have)
        character(len=:), allocatable, intent(out) :: prompt
        logical, intent(out) :: have

        have = allocated(minibuf_prompt)
        if (have) prompt = minibuf_prompt
    end subroutine minibuffer_prompt

    !> Whether BUF is the minibuffer of depth DEPTH.  Port of
    !> minibuf.c:is_minibuffer.
    logical function is_minibuffer(depth, buf)
        integer, intent(in) :: depth
        type(handle_type), intent(in) :: buf
        type(handle_type) :: m

        m = nth_minibuffer(depth)
        is_minibuffer = m%index /= 0 .and. m%index == buf%index .and. m%generation == buf%generation
    end function is_minibuffer

    !> Whether BUFFER is an active minibuffer (" *Minibuf-0*" never is).
    !> Port of minibuf.c:live_minibuffer_p.
    logical function live_minibuffer_p(buffer)
        type(handle_type), intent(in) :: buffer
        integer :: i

        live_minibuffer_p = .false.

        do i = 1, minibuf_level
            if (is_minibuffer(i, buffer)) then
                live_minibuffer_p = .true.
                return
            end if
        end do
    end function live_minibuffer_p

    !> The minibuffer of depth DEPTH, or the null handle if it does not
    !> exist yet.  Port of minibuf.c:nth_minibuffer.
    function nth_minibuffer(depth) result(buf)
        integer, intent(in) :: depth
        type(handle_type) :: buf

        buf = null_handle()
        if (.not. allocated(minibuffers)) return
        if (depth + 1 > size(minibuffers)) return
        buf = minibuffers(depth + 1)
    end function nth_minibuffer

    !> Run `minibuffer_exit_hook' with MINIBUF current, if it is live, as
    !> `safe_run_hooks' does.  Port of minibuf.c:run_exit_minibuf_hook.
    recursive subroutine run_exit_minibuf_hook(minibuf)
        type(handle_type), intent(in) :: minibuf
        type(handle_type) :: saved
        type(status_type) :: st

        saved = current_buffer()
        if (buffer_live_p(minibuf)) call set_buffer(minibuf, st)
        call safe_run_hooks(minibuffer_exit_hook)
        if (buffer_live_p(saved)) call set_buffer(saved, st)
    end subroutine run_exit_minibuf_hook

    !> Set up the minibuffer of depth 0 as minibuffer-inactive-mode would
    !> (its local variables killed, as set_minibuffer_mode does when
    !> that mode is not defined).  Port of
    !> minibuf.c:set_initial_minibuffer_mode with
    !> minibuf.c:set_minibuffer_mode (minibuffer-mode and
    !> minibuffer-inactive-mode are minibuffer.el's, not ported).
    subroutine set_initial_minibuffer_mode()
        type(handle_type) :: minibuf, saved
        type(status_type) :: st

        call get_minibuffer(0, minibuf, st)
        if (.not. st%ok()) return
        saved = current_buffer()
        call set_buffer(minibuf, st)
        call kill_all_local_variables()
        if (buffer_live_p(saved)) call set_buffer(saved, st)
    end subroutine set_initial_minibuffer_mode

    !> Whether STRING matches all the regexps of `completion_regexp_list',
    !> folding case when IGNORE_CASE ("Stack overflow in regexp matcher"
    !> is an error).  Port of minibuf.c:match_regexps.
    recursive logical function match_regexps(string, ignore_case, status) result(match)
        character(len=*), intent(in) :: string
        logical, intent(in) :: ignore_case
        type(status_type), intent(out) :: status
        integer :: k

        match = .true.
        if (.not. allocated(completion_regexp_list)) return

        do k = 1, size(completion_regexp_list)
            match = fast_string_match(trim(completion_regexp_list(k)), string, ignore_case, status) > 0
            if (.not. status%ok() .or. .not. match) return
        end do
    end function match_regexps

    !> Like `assoc' but specifically for strings (and symbols).
    !>
    !> This returns the first element of LIST whose car matches the string or
    !> symbol KEY, or nil if no match exists.  When performing the
    !> comparison, symbols are first converted to strings, and unibyte
    !> strings to multibyte.  If the optional arg CASE_FOLD is true, both
    !> KEY and the elements of LIST are upcased for comparison.
    !>
    !> Unlike `assoc', KEY can also match an entry in LIST consisting of a
    !> single string, rather than a cons cell whose car is a string.
    !> Port of minibuf.c:Fassoc_string (KEY as a string; a symbol is
    !> given by its name).
    function assoc_string(key, list_value, case_fold) result(elt)
        character(len=*), intent(in) :: key
        type(value_type), intent(in) :: list_value
        logical, intent(in), optional :: case_fold
        type(value_type) :: elt
        type(value_type), allocatable :: elts(:)
        type(value_type) :: thiscar
        character(len=:), allocatable :: s
        logical :: fold
        integer :: k

        elt = nil()
        fold = .false.
        if (present(case_fold)) fold = case_fold
        if (vtag(list_value) /= V_LIST) return
        call list_elements(list_value, elts)

        do k = 1, size(elts)
            thiscar = elts(k)
            if (vtag(elts(k)) == V_LIST) thiscar = list_elt(elts(k), 1_int64)

            if (symbolp(thiscar)) then
                s = symbols(xsymbol(thiscar))%name
            else if (stringp(thiscar)) then
                s = xstring(thiscar)
            else
                cycle
            end if

            if (compare_strings(s, key, fold) == COMPARE_EQUAL) then
                elt = elts(k)
                return
            end if
        end do
    end function assoc_string

    !> Read a string; empty input gives `default' if present.
    !> Port of minibuf.c:Fread_string.
    recursive subroutine read_string(prompt, result, status, initial, history, default, position)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default
        integer(int64), intent(in), optional :: position

        call read_from_minibuffer(prompt, result, status, initial=initial, history=history, &
                                  default=default, position=position)
        if (.not. status%ok()) return
        if (len(result) == 0 .and. present(default)) result = default
    end subroutine read_string

    !> Abort the current minibuffer.
    !> If we are not currently in the innermost minibuffer, prompt the user to
    !> confirm the aborting of the current minibuffer and all contained ones.
    !> Port of minibuf.c:Fabort_minibuffers (`ask' stands for
    !> `yes_or_no_p', which this module cannot use).
    recursive subroutine abort_minibuffers(status, ask)
        type(status_type), intent(out) :: status
        interface
            recursive subroutine ask(prompt, answer, status)
                import :: status_type
                implicit none(type, external)
                character(len=*), intent(in) :: prompt
                logical, intent(out) :: answer
                type(status_type), intent(out) :: status
            end subroutine ask
        end interface
        integer :: depth
        character(len=20) :: num
        logical :: answer

        status = status_type()
        depth = minibuffer_depth_of()

        if (depth == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a minibuffer")
            return
        end if

        if (.not. minibuffer_innermost_command_loop_p()) then
            call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
            return
        end if

        if (depth < minibuf_level) then
            write (num, "(i0)") minibuf_level - depth + 1
            call ask("Abort "//trim(num)//" minibuffer levels? ", answer, status)
            if (.not. status%ok()) return
            ! Due to the above check, the current minibuffer is in the
            ! most nested command loop, which means that we don't have to
            ! abort any extra non-minibuffer recursive edits.  Thus, the
            ! number of recursive edits we have to abort equals the number
            ! of minibuffers we have to abort.
            if (answer) call throw_exit(.true., minibuf_level - depth + 1)
        else
            ! minibuffer-quit-recursive-edit
            call throw_exit(.true.)
        end if
    end subroutine abort_minibuffers

    ! ------------------------------------------------------------------
    ! Completion primitives
    ! ------------------------------------------------------------------

    !> The elements of a non-function table: `elts' (the list elements or
    !> symbols, given to the predicate) and the string each one completes
    !> to (`names'; `ok' is false for elements that are not strings or
    !> symbols).  Not a port.
    subroutine table_elements(table, elts, names, ok)
        type(completion_table_type), intent(in) :: table
        type(value_type), allocatable, intent(out) :: elts(:)
        type(string_box), allocatable, intent(out) :: names(:)
        logical, allocatable, intent(out) :: ok(:)
        type(value_type), allocatable :: parts(:)
        type(value_type) :: eltstring
        integer(int64) :: k

        if (table%kind == TABLE_OBARRAY) then
            allocate (elts(nsymbols), names(nsymbols), ok(nsymbols))

            do k = 1, nsymbols
                elts(k) = make_symbol_ref(k)
                names(k)%s = symbols(k)%name
                ok(k) = .true.
            end do

            return
        end if

        call list_elements(table%list, elts)
        allocate (names(size(elts)), ok(size(elts)))

        do k = 1, size(elts, kind=int64)
            ! An alist element: its car is the completion.
            eltstring = elts(k)

            if (vtag(elts(k)) == V_LIST) then
                call list_elements(elts(k), parts)
                eltstring = nil()
                if (size(parts) > 0) eltstring = parts(1)
            end if

            ok(k) = .true.

            if (stringp(eltstring)) then
                names(k)%s = xstring(eltstring)
            else if (symbolp(eltstring)) then
                names(k)%s = symbols(xsymbol(eltstring))%name
            else
                ok(k) = .false.
                names(k)%s = ""
            end if
        end do
    end subroutine table_elements

    !> Whether completion candidate `name' begins with STRING.  Not a port.
    logical function prefix_matches(string, name)
        character(len=*), intent(in) :: string, name
        integer(int64) :: n

        n = schars(string)
        prefix_matches = n <= schars(name)
        if (prefix_matches) prefix_matches = compare_strings(name, string, completion_ignore_case%value, end1=n) &
                                             == COMPARE_EQUAL
    end function prefix_matches

    !> Return longest common substring of all completions of STRING in
    !> COLLECTION.
    !>
    !> Test each possible completion specified by COLLECTION to see if it
    !> begins with STRING.  The possible completions may be strings or
    !> symbols.  Symbols are converted to strings before testing, by using
    !> `symbol_name'.
    !>
    !> If no possible completions match, the function returns nil; if
    !> there's just one exact match, it returns t; otherwise it returns
    !> the longest initial substring common to all possible completions
    !> that begin with STRING.
    !>
    !> If COLLECTION is an alist, the keys (cars of elements) are the
    !> possible completions.  If an element is not a cons cell, then the
    !> element itself is a possible completion.  If COLLECTION is the
    !> obarray, the names of all symbols in the obarray are the possible
    !> completions.
    !>
    !> COLLECTION can also be a function to do the completion itself.  It
    !> receives STRING, PREDICATE and COMPLETE_TRY.  Whatever it returns
    !> becomes the value of `try_completion'.
    !>
    !> If optional argument PREDICATE is present, it is used to test each
    !> possible completion.  A possible completion is accepted only if
    !> PREDICATE returns true.  The argument given to PREDICATE is either a
    !> string or a list (whose first element is a string) from the alist,
    !> or a symbol from the obarray.
    !>
    !> If `completion_ignore_case' is non-nil, possible completions are
    !> matched while ignoring letter-case, but no guarantee is made about
    !> the letter-case of the return value, except that it comes either
    !> from the user's input or from one of the possible completions.
    !> Port of minibuf.c:Ftry_completion.
    recursive subroutine try_completion(string, collection, result, status, predicate)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        character(len=:), allocatable :: bestmatch, old_bestmatch, eltstring
        integer(int64) :: bestmatchsize, compare, matchsize, tem, k, nstring, neltstring, nbest
        integer :: matchcount
        logical :: have_best

        status = status_type()
        result = nil()

        if (collection%kind == TABLE_FUNCTION) then
            call collection%fn(string, COMPLETE_TRY, result, status, predicate)
            return
        else if (collection%kind == TABLE_QUOTING) then
            call quoting_table_call(collection, string, COMPLETE_TRY, result, status, predicate)
            return
        end if

        call table_elements(collection, elts, names, ok)
        have_best = .false.
        bestmatchsize = 0
        matchcount = 0
        nstring = schars(string)

        do k = 1, size(elts, kind=int64)
            ! Is this element a possible completion?
            if (.not. ok(k)) cycle
            eltstring = names(k)%s
            if (.not. prefix_matches(string, eltstring)) cycle

            ! Ignore this element if it fails to match all the regexps.
            if (.not. match_regexps(eltstring, completion_ignore_case%value, status)) then
                if (.not. status%ok()) return
                cycle
            end if

            ! Ignore this element if there is a predicate and the
            ! predicate doesn't like it.
            if (present(predicate)) then
                if (.not. predicate(elts(k))) cycle
            end if

            neltstring = schars(eltstring)

            ! Update computation of how much all possible completions match.
            if (.not. have_best) then
                matchcount = 1
                bestmatch = eltstring
                bestmatchsize = neltstring
                have_best = .true.
            else
                compare = min(bestmatchsize, neltstring)
                tem = compare_strings(bestmatch, eltstring, completion_ignore_case%value, end1=compare, &
                                      end2=compare)
                matchsize = compare
                if (tem /= COMPARE_EQUAL) matchsize = abs(tem) - 1
                old_bestmatch = bestmatch
                nbest = schars(bestmatch)

                if (completion_ignore_case%value) then
                    ! If this is an exact match except for case, use it as
                    ! the best match rather than one that is not an exact
                    ! match.  This way, we get the case pattern of the
                    ! actual match.  If there is more than one exact match
                    ! ignoring case, and one of them is exact including
                    ! case, prefer that one.  If there is no exact match
                    ! ignoring case, prefer a match that does not change
                    ! the case of the input.
                    if ((matchsize == neltstring .and. matchsize < nbest) .or. &
                        ((matchsize == neltstring) .eqv. (matchsize == nbest)) .and. &
                        compare_strings(eltstring, string, end1=nstring) == COMPARE_EQUAL .and. &
                        compare_strings(bestmatch, string, end1=nstring) /= COMPARE_EQUAL) then
                        bestmatch = eltstring
                    end if
                end if

                ! Don't count the same string multiple times.
                if (bestmatchsize /= neltstring .or. bestmatchsize /= matchsize) then
                    if (matchcount <= 1) matchcount = matchcount + 1
                else if (completion_ignore_case%value) then
                    if (compare_strings(old_bestmatch, eltstring, end1=compare, end2=compare) /= COMPARE_EQUAL &
                        .and. matchcount <= 1) matchcount = matchcount + 1
                end if

                bestmatchsize = matchsize
                ! No need to look any further (unless ignoring case, to find
                ! the best possible match *including* case differences).
                if (matchsize <= nstring .and. .not. completion_ignore_case%value .and. matchcount > 1) exit
            end if
        end do

        ! No completions found.
        if (.not. have_best) return

        ! Return t if the supplied string is an exact match (counting
        ! case); it does not require any change to be made.
        if (matchcount == 1 .and. bestmatch == string .and. len(bestmatch) == len(string)) then
            result = t_value()
            return
        end if

        ! Else extract the part in which all completions agree.
        result = make_string(substring(bestmatch, 0_int64, bestmatchsize))
    end subroutine try_completion

    !> Search for partial matches of STRING in COLLECTION.
    !>
    !> Test each possible completion specified by COLLECTION to see if it
    !> begins with STRING.  The possible completions may be strings or
    !> symbols.  Symbols are converted to strings before testing, by using
    !> `symbol_name'.
    !>
    !> The value is a list of all the possible completions that match
    !> STRING.
    !>
    !> If COLLECTION is an alist, the keys (cars of elements) are the
    !> possible completions.  If COLLECTION is the obarray, the names of
    !> all symbols in the obarray are the possible completions.
    !>
    !> COLLECTION can also be a function to do the completion itself.  It
    !> receives STRING, PREDICATE and COMPLETE_ALL.  Whatever it returns
    !> becomes the value of `all_completions'.
    !>
    !> If optional argument PREDICATE is present, it is used to test each
    !> possible completion.  A possible completion is accepted only if
    !> PREDICATE returns true.  Port of minibuf.c:Fall_completions.
    recursive subroutine all_completions(string, collection, result, status, predicate)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:), matches(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        integer(int64) :: k, n

        status = status_type()

        if (collection%kind == TABLE_FUNCTION) then
            call collection%fn(string, COMPLETE_ALL, result, status, predicate)
            return
        else if (collection%kind == TABLE_QUOTING) then
            call quoting_table_call(collection, string, COMPLETE_ALL, result, status, predicate)
            return
        end if

        call table_elements(collection, elts, names, ok)
        allocate (matches(size(elts)))
        n = 0

        do k = 1, size(elts, kind=int64)
            ! Is this element a possible completion?
            if (.not. ok(k)) cycle
            if (.not. prefix_matches(string, names(k)%s)) cycle

            ! Ignore this element if it fails to match all the regexps.
            if (.not. match_regexps(names(k)%s, completion_ignore_case%value, status)) then
                if (.not. status%ok()) return
                cycle
            end if

            ! Ignore this element if there is a predicate and the
            ! predicate doesn't like it.
            if (present(predicate)) then
                if (.not. predicate(elts(k))) cycle
            end if

            ! Ok => put it on the list.
            n = n + 1
            matches(n) = make_string(names(k)%s)
        end do

        result = make_list(matches(1:n))
    end subroutine all_completions

    !> Return non-nil if STRING is a valid completion.
    !> For instance, if COLLECTION is a list of strings, STRING is a valid
    !> completion if it appears in the list and PREDICATE is satisfied.
    !>
    !> Takes the same arguments as `all_completions' and `try_completion'.
    !>
    !> If COLLECTION is a function, it is called with STRING, PREDICATE and
    !> COMPLETE_TEST (`lambda').  Port of minibuf.c:Ftest_completion.
    recursive logical function test_completion(string, collection, predicate) result(valid)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        type(value_type) :: tem, result
        type(status_type) :: status
        integer(int64) :: k, id

        valid = .false.

        select case (collection%kind)
        case (TABLE_FUNCTION)
            call collection%fn(string, COMPLETE_TEST, result, status, predicate)
            valid = status%ok() .and. .not. nilp(result)
            return
        case (TABLE_QUOTING)
            call quoting_table_call(collection, string, COMPLETE_TEST, result, status, predicate)
            valid = status%ok() .and. .not. nilp(result)
            return
        case (TABLE_OBARRAY)
            id = intern_soft(string)

            if (id /= 0) then
                tem = make_symbol_ref(id)
            else if (completion_ignore_case%value) then
                do k = 1, nsymbols
                    if (compare_strings(string, symbols(k)%name, .true.) == COMPARE_EQUAL) then
                        id = k
                        tem = make_symbol_ref(k)
                        exit
                    end if
                end do
            end if

            if (id == 0) return
        case default
            ! assoc-string
            call table_elements(collection, elts, names, ok)
            id = 0

            do k = 1, size(elts, kind=int64)
                if (.not. ok(k)) cycle

                if (compare_strings(string, names(k)%s, completion_ignore_case%value) == COMPARE_EQUAL) then
                    id = k
                    tem = elts(k)
                    exit
                end if
            end do

            if (id == 0) return
        end select

        ! Reject this element if it fails to match all the regexps.
        if (.not. match_regexps(string, completion_ignore_case%value, status)) return
        ! Finally, check the predicate.
        valid = .true.
        if (present(predicate)) valid = predicate(tem)
    end function test_completion

    ! ------------------------------------------------------------------
    ! Completion tables operating on quoted text
    ! ------------------------------------------------------------------

    !> The completion table of `completion_table_with_quoting': ACTION
    !> for the quoted STRING.  A difficult part of
    !> completion-with-quoting is to map positions in the quoted string to
    !> equivalent positions in the unquoted string and vice-versa.  There
    !> is no efficient and reliable algorithm that works for arbitrary
    !> quote and unquote functions.  So to map from quoted positions to
    !> unquoted positions, we simply assume that `concat' and `unquote'
    !> commute (which tends to be the case).  And we ask `requote' to do
    !> the work of mapping from unquoted positions back to quoted
    !> positions.  Port of the lambda of
    !> minibuffer.el:completion-table-with-quoting (no metadata, and no
    !> `completion--unquote' action: the completion styles call
    !> `try_completion' and `all_completions' on the quoted table).
    recursive subroutine quoting_table_call(table, string, action, result, status, predicate, suffix)
        type(completion_table_type), intent(in) :: table
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: inner
        procedure(completion_quote_proc), pointer :: qfun
        type(value_type) :: completion
        character(len=:), allocatable :: ustring, qsuffix, ufull, usuffix, qstring
        integer(int64) :: bstart, bend, qlboundary, qrboundary, q, qpoint

        status = status_type()
        result = nil()
        inner%kind = TABLE_FUNCTION
        inner%fn => table%fn
        call table%unquote(string, ustring)

        select case (action)
        case (COMPLETE_TEST)
            ! test-completion
            if (test_completion(ustring, inner, predicate)) result = t_value()
        case (COMPLETE_BOUNDARIES)
            qsuffix = ""
            if (present(suffix)) qsuffix = suffix

            if (len(qsuffix) == 0) then
                ufull = ustring
            else
                call table%unquote(string//qsuffix, ufull)
            end if

            ! If (not (string-prefix-p ustring ufull)) we have a problem:
            ! unquoting the qfull gives something "unrelated" to ustring.
            ! E.g. "~/" and "/" where "~//" gets unquoted to just "/" (see
            ! bug#47678).  In that case we can't even tell if we're right
            ! before the "/" or right after it (aka if this "/" is from
            ! qstring or from qsuffix), thus which usuffix to use is very
            ! unclear.
            if (string_prefix_p(ustring, ufull)) then
                usuffix = substring(ufull, schars(ustring))
            else
                usuffix = qsuffix
            end if

            call inner_boundaries(inner, ustring, usuffix, bstart, bend, predicate)
            call table%requote(bstart, string, qlboundary, qfun)

            if (bend == 0) then
                ! Common case.
                qrboundary = 0
            else
                call table%requote(bend + schars(ustring), string//qsuffix, q, qfun)
                qrboundary = q - schars(string)
            end if

            result = list(make_fixnum(qlboundary), make_fixnum(qrboundary))
        case (COMPLETE_TRY)
            ! try-completion: most forms of quoting allow several ways to
            ! quote the same string.  So here we could simply requote
            ! `completion' in a kind of "canonical" quoted form without
            ! paying attention to the way `string' was quoted.  But since
            ! we have to solve the more complex problems of "pay attention
            ! to the original quoting" for all-completions, we may as well
            ! use it here, since it provides a nicer behavior.
            call try_completion(ustring, inner, completion, status, predicate)
            if (.not. status%ok()) return

            if (.not. stringp(completion)) then
                result = completion
            else
                call twq_try(table, string, ustring, xstring(completion), 0_int64, qstring, qpoint)
                result = make_string(qstring)
            end if
        case (COMPLETE_ALL)
            ! all-completions
            call all_completions(ustring, inner, completion, status, predicate)
            if (.not. status%ok()) return
            call inner_boundaries(inner, ustring, "", bstart, bend, predicate)
            call twq_all(table, string, ustring, completion, bstart, result)
        case default
            continue
        end select
    end subroutine quoting_table_call

    ! The boundaries of the function table INNER for STRING and SUFFIX
    ! (completion-boundaries).  Not a port.
    recursive subroutine inner_boundaries(inner, string, suffix, bstart, bend, predicate)
        type(completion_table_type), intent(in) :: inner
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(out) :: bstart, bend
        procedure(completion_predicate), optional :: predicate
        type(value_type) :: boundaries
        type(status_type) :: status

        bstart = 0
        bend = schars(suffix)
        call inner%fn(string, COMPLETE_BOUNDARIES, boundaries, status, predicate, suffix)
        if (.not. status%ok() .or. list_length(boundaries) /= 2) return
        if (fixnump(list_elt(boundaries, 1_int64))) bstart = xfixnum(list_elt(boundaries, 1_int64))
        if (fixnump(list_elt(boundaries, 2_int64))) bend = xfixnum(list_elt(boundaries, 2_int64))
    end subroutine inner_boundaries

    ! Whether PREFIX is a prefix of STRING (string-prefix-p).  Not a port.
    logical function string_prefix_p(prefix, string)
        character(len=*), intent(in) :: prefix, string

        string_prefix_p = .false.
        if (len(prefix) > len(string)) return
        string_prefix_p = string(1:len(prefix)) == prefix
    end function string_prefix_p

    ! The longest common prefix of S1 and S2
    ! (fill-common-string-prefix).  Not a port.
    function fill_common_string_prefix(s1, s2) result(prefix)
        character(len=*), intent(in) :: s1, s2
        character(len=:), allocatable :: prefix
        integer(int64) :: n, k

        n = min(schars(s1), schars(s2))

        do k = 0, n - 1
            if (substring(s1, k, k + 1) /= substring(s2, k, k + 1)) then
                prefix = substring(s1, 0_int64, k)
                return
            end if
        end do

        prefix = substring(s1, 0_int64, n)
    end function fill_common_string_prefix

    ! Basically two cases: either the new result is
    ! - commonprefix1 <point> morecommonprefix <qpos> suffix
    ! - commonprefix <qpos> newprefix <point> suffix
    ! Port of minibuffer.el:completion--twq-try.
    recursive subroutine twq_try(table, string, ustring, completion, point, qstring, qpoint)
        type(completion_table_type), intent(in) :: table
        character(len=*), intent(in) :: string, ustring, completion
        integer(int64), intent(in) :: point
        character(len=:), allocatable, intent(out) :: qstring
        integer(int64), intent(out) :: qpoint
        procedure(completion_quote_proc), pointer :: qfun, qfun2
        character(len=:), allocatable :: prefix, suffix, qstr1, qsuffix, qstr, uqstring
        integer(int64) :: qpos, q

        prefix = fill_common_string_prefix(ustring, completion)
        suffix = substring(completion, max(point, schars(prefix)))
        call table%requote(schars(prefix), string, qpos, qfun)
        qstr1 = ""
        if (point > schars(prefix)) call qfun(substring(completion, schars(prefix), point), qstr1)
        call qfun(suffix, qsuffix)
        qstring = substring(string, 0_int64, qpos)//qstr1//qsuffix

        if (point == 0) then
            qpoint = 0
        else if (point > schars(prefix)) then
            qpoint = qpos + schars(qstr1)
        else
            call table%requote(point, string, q, qfun2)
            qpoint = q
        end if

        ! Make sure `requote' worked.
        call table%unquote(qstring, uqstring)

        if (uqstring /= completion .or. len(uqstring) /= len(completion)) then
            ! If requote failed (e.g. because sifn-requote did not handle
            ! Tramp's "/foo:/bar//baz -> /foo:/baz" truncation), then at
            ! least try requote properly.
            call qfun(completion, qstr)
            qstring = qstr
            qpoint = schars(qstr)
        end if
    end subroutine twq_try

    ! The COMPLETIONS of the unquoted string USTRING, after BOUNDARY,
    ! quoted as STRING is.  Port of minibuffer.el:completion--twq-all (no
    ! `completions-common-part' face on the quoted prefix, no
    ! `completion--unquoted' property; the base size is the boundary).
    recursive subroutine twq_all(table, string, ustring, completions, boundary, result)
        type(completion_table_type), intent(in) :: table
        character(len=*), intent(in) :: string, ustring
        type(value_type), intent(in) :: completions
        integer(int64), intent(in) :: boundary
        type(value_type), intent(out) :: result
        type(value_type), allocatable :: elems(:), quoted(:)
        type(value_type) :: tried
        type(completion_table_type) :: prefix_table
        procedure(completion_quote_proc), pointer :: qfun, qfun2
        type(status_type) :: st
        character(len=:), allocatable :: prefix, qfullprefix, qprefix, qp, completion, new, qnew, rest, qrest
        integer(int64) :: qfullpos, qboundary, k, n

        result = nil()
        n = list_length(completions)
        if (n == 0) return
        call list_elements(completions, elems)
        ! (try-completion "" (cons (substring ustring boundary) completions))
        allocate (quoted(n + 1))
        quoted(1) = make_string(substring(ustring, boundary))

        do k = 1, n
            quoted(k + 1) = elems(k)
        end do

        prefix_table = completion_list_table(make_list(quoted))
        call try_completion("", prefix_table, tried, st)

        if (stringp(tried)) then
            prefix = xstring(tried)
        else
            prefix = substring(ustring, boundary)
        end if

        call table%requote(boundary + schars(prefix), string, qfullpos, qfun)
        qfullprefix = substring(string, 0_int64, qfullpos)
        call table%requote(boundary, string, qboundary, qfun2)
        qprefix = substring(qfullprefix, qboundary)
        ! Here we choose to quote all elements returned, but a better
        ! option would be to return unquoted elements together with a
        ! function to requote them, so that *Completions* can show nicer
        ! unquoted values which only get quoted when needed by
        ! choose-completion.
        deallocate (quoted)
        allocate (quoted(n))

        do k = 1, n
            completion = xstring(elems(k))
            new = substring(completion, schars(prefix))
            call qfun(new, qnew)
            qp = qprefix

            if (completion_ignore_case%value) then
                ! Make qprefix inherit the case from `completion'.
                rest = substring(completion, 0_int64, schars(prefix))
                call qfun(rest, qrest)
                if (compare_strings(qprefix, qrest, .true.) == COMPARE_EQUAL) qp = qrest
            end if

            quoted(k) = make_string(qp//qnew)
        end do

        result = make_list(quoted)
    end subroutine twq_all

    !> Perform completion according to ACTION.
    !> STRING, TABLE and PREDICATE are used as in `try_completion'.
    !>
    !> If TABLE is a function, it will be called directly to perform
    !> completion, no matter what ACTION is.
    !>
    !> If ACTION is COMPLETE_BOUNDARIES, return nil (the default
    !> boundaries) for other tables.  If ACTION is COMPLETE_TRY, this
    !> works like `try_completion'; if it is COMPLETE_ALL, like
    !> `all_completions'; and COMPLETE_TEST makes it work like
    !> `test_completion'.  Port of minibuffer.el:complete-with-action.
    recursive subroutine complete_with_action(action, table, string, result, status, predicate, suffix)
        integer, intent(in) :: action
        type(completion_table_type), intent(in) :: table
        character(len=*), intent(in) :: string
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        character(len=*), intent(in), optional :: suffix

        status = status_type()
        result = nil()

        if (table%kind == TABLE_FUNCTION) then
            call table%fn(string, action, result, status, predicate, suffix)
            return
        end if

        if (table%kind == TABLE_QUOTING) then
            call quoting_table_call(table, string, action, result, status, predicate, suffix)
            return
        end if

        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, table, result, status, predicate)
        case (COMPLETE_ALL)
            call all_completions(string, table, result, status, predicate)
        case (COMPLETE_TEST)
            if (test_completion(string, table, predicate)) result = t_value()
        case default
            continue
        end select
    end subroutine complete_with_action

    !> Complete with TABLE, with S1 replaced by S2: strings of the form
    !> (concat S1 S) complete in the same way as TABLE completes strings
    !> of the form (concat S2 S).  ACTION, STRING, RESULT, PREDICATE and
    !> SUFFIX are those of a function table.  Port of the lambda of
    !> minibuffer.el:completion-table-subvert.
    recursive subroutine completion_table_subvert(table, s1, s2, string, action, result, status, predicate, suffix)
        type(completion_table_type), intent(in) :: table
        character(len=*), intent(in) :: s1, s2, string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        character(len=*), intent(in), optional :: suffix
        type(value_type) :: res
        type(value_type), allocatable :: elts(:), kept(:)
        character(len=:), allocatable :: str, re, c
        integer(int64) :: beg, bstart, bend, n, k
        logical :: has_str

        status = status_type()
        result = nil()
        has_str = prefix_p(s1, string, completion_ignore_case%value)
        res = nil()

        if (has_str) then
            str = s2//substring(string, schars(s1))
            call complete_with_action(action, table, str, res, status, predicate, suffix)
            if (.not. status%ok()) return
        end if

        if (action == COMPLETE_BOUNDARIES) then
            beg = 0

            if (list_length(res) == 2) then
                if (fixnump(list_elt(res, 1_int64))) beg = xfixnum(list_elt(res, 1_int64))
                result = list(make_fixnum(min(schars(string), max(schars(s1), beg + schars(s1) - schars(s2)))), &
                              list_elt(res, 2_int64))
            else
                result = list(make_fixnum(min(schars(string), max(schars(s1), beg + schars(s1) - schars(s2)))), &
                              nil())
            end if

            return
        end if

        if (nilp(res)) return

        if (stringp(res)) then
            if (prefix_p(s2, xstring(res), completion_ignore_case%value)) then
                result = make_string(s1//substring(xstring(res), schars(s2)))
            end if
        else if (action == COMPLETE_ALL) then
            call inner_boundaries(table, str, "", bstart, bend, predicate)

            if (bstart >= schars(s2)) then
                result = res
            else
                re = "\`"//regexp_quote(substring(s2, bstart))
                call list_elements(res, elts)
                allocate (kept(size(elts)))
                n = 0

                do k = 1, size(elts)
                    c = xstring(elts(k))

                    if (string_match(re, c, status) > 0) then
                        n = n + 1
                        kept(n) = make_string(substring(c, match_end(0, status) - 1))
                    end if
                end do

                status = status_type()
                result = make_list(kept(1:n))
            end if
        else
            ! E.g. action=nil and it's the only completion.
            result = res
        end if
    end subroutine completion_table_subvert

    !> Complete with TABLE filtered through PRED1: PRED1 is a function of
    !> one argument which returns true if and only if the argument is an
    !> element of TABLE which should be considered for completion.
    !> STRING, PREDICATE (PRED2) and ACTION are the usual arguments to
    !> completion tables.  If STRICT is true, the predicate always
    !> applies; if false it only applies if it does not reduce the set of
    !> possible completions to nothing.  Note: TABLE needs to be a proper
    !> completion table which obeys predicates.  Port of
    !> minibuffer.el:completion-table-with-predicate.
    recursive subroutine completion_table_with_predicate(table, pred1, strict, string, action, result, status, &
                                                         predicate, suffix)
        type(completion_table_type), intent(in) :: table
        procedure(completion_predicate), optional :: pred1
        logical, intent(in) :: strict
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        character(len=*), intent(in), optional :: suffix
        procedure(completion_predicate), pointer :: saved1, saved2

        status = status_type()
        result = nil()

        if (.not. strict .and. action == COMPLETE_TEST) then
            ! Ignore pred1 since it doesn't really have to apply anyway.
            if (test_completion(string, table, predicate)) result = t_value()
            return
        end if

        if (present(pred1) .and. present(predicate)) then
            ! Call `pred1' first, so that `pred2' really can't tell that
            ! `x' is in table.
            saved1 => and_pred1
            saved2 => and_pred2
            and_pred1 => pred1
            and_pred2 => predicate
            call complete_with_action(action, table, string, result, status, both_preds, suffix)
            and_pred1 => saved1
            and_pred2 => saved2
        else if (present(pred1)) then
            call complete_with_action(action, table, string, result, status, pred1, suffix)
        else
            call complete_with_action(action, table, string, result, status, predicate, suffix)
        end if

        if (.not. status%ok()) return

        ! If completion failed and we're not applying pred1 strictly, try
        ! again without pred1.
        if (nilp(result) .and. .not. strict .and. present(pred1)) then
            call complete_with_action(action, table, string, result, status, predicate, suffix)
        end if
    end subroutine completion_table_with_predicate

    ! Both predicates of `completion_table_with_predicate'.  Not a port.
    recursive logical function both_preds(elt)
        type(value_type), intent(in) :: elt
        procedure(completion_predicate), pointer :: p2

        p2 => and_pred2
        both_preds = and_pred1(elt)
        if (both_preds) both_preds = p2(elt)
    end function both_preds

    ! Whether PREFIX is a prefix of STRING, ignoring case if IGNORE_CASE
    ! (string-prefix-p).  Not a port.
    logical function prefix_p(prefix, string, ignore_case)
        character(len=*), intent(in) :: prefix, string
        logical, intent(in) :: ignore_case

        prefix_p = .false.
        if (schars(prefix) > schars(string)) return
        prefix_p = compare_strings(prefix, substring(string, 0_int64, schars(prefix)), ignore_case) == COMPARE_EQUAL
    end function prefix_p
end module minibuf_fm
