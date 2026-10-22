! isearch_fm.f90 -- incremental search minor mode.
! Ported from GNU Emacs 31.1 lisp/isearch.el.
!
! Copyright (C) 1992-1997, 1999-2026 Free Software Foundation, Inc.
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

!> Incremental search: C-s and C-r.  As upstream, `isearch_mode` makes
!> `isearch-mode-map` the overriding terminal-local map and installs a
!> pre-command hook; keys bound there extend or move the search, and any
!> other command ends the search first and then runs as usual.
!>
!> Literal search only: no regexp, word, symbol or char-fold search, and
!> no lax whitespace matching (they need regexp search).  Not ported yet:
!> highlighting the match and lazy highlighting (faces from overlays),
!> C-w and C-M-w (word and symbol syntax), reading a kill with the
!> minibuffer (`isearch-yank-from-kill-ring`, C-u M-y), search through invisible text, scrolling during search
!> (`isearch-allow-scroll`), `isearch-allow-motion`, query-replace and
!> occur from isearch, the help map, input methods, completion, the
!> minibuffer's own isearch keys while editing the string.
module isearch_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_QUIT, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, make_fixnum, fixnump, xfixnum, stringp, xstring, make_keymap_ref, &
                       list_length, list_elt, list_elements, make_list, nilp
    use lread_fm, only: sym
    use data_fm, only: value_eq, symbol_value, set
    use character_fm, only: string_char_and_length, char_string, MAX_CHAR, MAX_MULTIBYTE_LENGTH, &
                            CHAR_SHIFT
    use casetab_fm, only: downcase
    use buffer_fm, only: buffers, cur, transient_mark_mode
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, eolp, pos_eol, &
                          buffer_substring
    use cmds_fm, only: forward_char
    use search_fm, only: search_forward, search_backward, match_beginning, match_end, &
                         case_fold_search, SEARCH_NOERROR
    use keymap_fm, only: make_keymap, make_sparse_keymap, define_key, define_key_range, key_parse, &
                         lookup_key, keymap_live_p, free_keymap, overriding_terminal_local_map
    use keyboard_fm, only: this_command, last_command, last_command_event, this_command_keys, &
                           pre_command_hook, redisplay_proc, discard_input, input_pending_p, &
                           read_event
    use callint_fm, only: defcommand, commandp, current_prefix_arg, prefix_numeric_value
    use subr_fm, only: add_to_history
    use minibuf_fm, only: read_from_minibuffer, history_add_new_input, minibuf_level
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use simple_fm, only: push_mark, current_kill
    implicit none(type, external)
    private

    public :: isearch_forward_command, isearch_backward_command, isearch_mode, isearch_done, &
              isearch_update, isearch_exit, isearch_cancel, isearch_abort, &
              isearch_repeat_forward, isearch_repeat_backward, isearch_delete_char, &
              isearch_del_char, isearch_printing_char, isearch_process_search_string, &
              isearch_yank_string, isearch_yank_char, isearch_yank_line, isearch_yank_kill, isearch_yank_pop_only, &
              isearch_beginning_of_buffer, isearch_end_of_buffer, isearch_ring_advance, &
              isearch_ring_retreat, isearch_toggle_case_fold, isearch_edit_string, &
              isearch_message_prefix, syms_of_isearch

    !> Values of `isearch_case_fold_search`: nil, t, and `yes` (set by
    !> M-c: fold even when the string has upper-case letters).
    integer, parameter, public :: CASE_FOLD_NIL = 0, CASE_FOLD_T = 1, CASE_FOLD_YES = 2

    !> Values of `search_upper_case`: nil, t and `not-yanks`.
    integer, parameter, public :: UPPER_CASE_NIL = 0, UPPER_CASE_T = 1, UPPER_CASE_NOT_YANKS = 2

    !> If non-nil, upper case chars disable case fold searching.
    !> That is, upper and lower case chars must match exactly.
    !> This applies no matter where the chars come from, but does not
    !> apply to chars in regexps that are prefixed with `\'.
    !> If this value is `not-yanks', text yanked into the search string
    !> in Isearch mode is always downcased.  Port of isearch.el
    !> search-upper-case.
    integer, public :: search_upper_case = UPPER_CASE_NOT_YANKS

    !> Non-nil means random control characters terminate incremental
    !> search.  (Only t and nil: `edit`, `append`, `move` and
    !> `shift-move` are not ported.)  Port of isearch.el search-exit-option.
    logical, public :: search_exit_option = .true.

    !> If non-nil, do a nonincremental search instead of an incremental
    !> one.  This applies to RET typed immediately after entering
    !> incremental search.  Port of isearch.el
    !> search-nonincremental-instead.
    logical, public :: search_nonincremental_instead = .true.

    !> Maximum length of search ring before oldest elements are thrown
    !> away.  Port of isearch.el search-ring-max.
    integer, public :: search_ring_max = 16

    !> isearch.el isearch-mode: whether Isearch mode is on.
    logical, public, protected :: isearch_active = .false.

    !> Keymap for `isearch-mode'.  Port of isearch.el isearch-mode-map.
    type(handle_type), public, protected :: isearch_mode_map

    !> The search state, as isearch.el's global variables.  Strings are
    !> UTF-8; an unallocated `isearch_error` is nil; position 0 is nil.
    character(len=:), allocatable, public, protected :: isearch_string, isearch_message
    character(len=:), allocatable, public, protected :: isearch_error
    logical, public, protected :: isearch_forward = .true., isearch_success = .true.
    logical, public, protected :: isearch_wrapped = .false.
    integer(int64), public, protected :: isearch_other_end = 0, isearch_opoint = 0, isearch_barrier = 0
    integer, public, protected :: isearch_case_fold_search = CASE_FOLD_T
    integer :: isearch_last_case_fold_search = CASE_FOLD_T
    logical :: isearch_adjusted = .false., isearch_yank_flag = .false.
    logical :: isearch_just_started = .false., isearch_nonincremental = .false.
    integer(int64) :: isearch_yank_prev_point = 0

    !> isearch.el search-ring-yank-pointer: 0-based, -1 for nil.
    integer :: search_ring_yank_pointer = -1

    !> One element of isearch-cmds (isearch.el isearch--state).
    type :: isearch_state
        character(len=:), allocatable :: string, message, error
        integer(int64) :: point = 0, other_end = 0, barrier = 0
        logical :: success = .true., forward = .true., wrapped = .false.
        integer :: case_fold = CASE_FOLD_T
    end type isearch_state

    !> isearch-cmds: the states, newest last.
    type(isearch_state), allocatable :: cmds(:)
    integer :: ncmds = 0

    !> isearch--saved-local-map and isearch--saved-overriding-local-map.
    type(handle_type) :: saved_local_map, saved_overriding_local_map
    !> The pre-command hook isearch-mode replaced.
    procedure(redisplay_proc), pointer :: saved_pre_command_hook => null()

contains

    ! ------------------------------------------------------------------
    ! Strings
    ! ------------------------------------------------------------------

    !> The characters of UTF-8 `s`.
    subroutine decode(s, chars, n)
        character(len=*), intent(in) :: s
        integer(int32), allocatable, intent(out) :: chars(:)
        integer, intent(out) :: n
        integer(int64) :: i
        integer(int32) :: c, clen
        allocate (chars(len(s)))
        n = 0
        i = 1
        do while (i <= len(s))
            call string_char_and_length(s, i, c, clen)
            n = n + 1
            chars(n) = c
            i = i + clen
        end do
    end subroutine decode

    !> The UTF-8 encoding of character `c`.
    function encode(c) result(s)
        integer(int32), intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        type(status_type) :: status
        call char_string(c, buf, nbytes, status)
        s = buf(1:nbytes)
    end function encode

    !> Number of characters in UTF-8 `s`.
    integer function nchars(s)
        character(len=*), intent(in) :: s
        integer(int32), allocatable :: chars(:)
        call decode(s, chars, nchars)
    end function nchars

    !> `s` without its last `n` characters.
    function drop_chars(s, n) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        call decode(s, chars, m)
        r = ""
        do k = 1, m - n
            r = r//encode(chars(k))
        end do
    end function drop_chars

    !> Port of casefiddle.c:Fdowncase for a string (character by
    !> character through the case table).
    function downcase_string(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        call decode(s, chars, m)
        r = ""
        do k = 1, m
            r = r//encode(downcase(chars(k)))
        end do
    end function downcase_string

    !> Return t if there are no upper case chars in STRING.
    !> Port of isearch.el:isearch-no-upper-case-p (REGEXP-FLAG nil).
    logical function isearch_no_upper_case_p(string) result(none)
        character(len=*), intent(in) :: string
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        call decode(string, chars, m)
        none = .true.
        do k = 1, m
            if (downcase(chars(k)) /= chars(k)) then
                none = .false.
                return
            end if
        end do
    end function isearch_no_upper_case_p

    ! ------------------------------------------------------------------
    ! The search ring
    ! ------------------------------------------------------------------

    !> The search ring (isearch.el search-ring), most recent first.
    subroutine search_ring(elems)
        type(value_type), allocatable, intent(out) :: elems(:)
        type(value_type) :: ring
        type(status_type) :: status
        call symbol_value(sym("search-ring"), ring, status)
        if (.not. status%ok()) ring = nil()
        call list_elements(ring, elems)
    end subroutine search_ring

    !> Add STRING to the beginning of the search ring.
    !> Port of isearch.el:isearch-update-ring (REGEXP nil).
    subroutine isearch_update_ring(string)
        character(len=*), intent(in) :: string
        type(value_type), allocatable :: elems(:)
        type(status_type) :: status
        call add_to_history("search-ring", string)
        call search_ring(elems)
        if (size(elems) > search_ring_max) call set(sym("search-ring"), make_list(elems(1:search_ring_max)), status)
    end subroutine isearch_update_ring

    !> Port of isearch.el:isearch-ring-adjust1.
    subroutine isearch_ring_adjust1(advance)
        logical, intent(in) :: advance
        type(value_type), allocatable :: ring(:)
        integer :: length, yank_pointer
        call search_ring(ring)
        length = size(ring)
        if (length == 0) return
        yank_pointer = search_ring_yank_pointer
        if (yank_pointer < 0) yank_pointer = merge(0, -1, advance)
        yank_pointer = modulo(yank_pointer + merge(-1, 1, advance), length)
        search_ring_yank_pointer = yank_pointer
        if (stringp(ring(yank_pointer + 1))) then
            isearch_string = xstring(ring(yank_pointer + 1))
            isearch_message = isearch_string
        end if
    end subroutine isearch_ring_adjust1

    !> Port of isearch.el:isearch-ring-adjust (search-ring-update nil:
    !> edit the string in the minibuffer).
    subroutine isearch_ring_adjust(advance, status)
        logical, intent(in) :: advance
        type(status_type), intent(out) :: status
        call isearch_ring_adjust1(advance)
        ! Otherwise, edit the search string instead.
        call isearch_edit_string(status)
    end subroutine isearch_ring_adjust

    !> Advance to the next search string in the ring.  Port of
    !> isearch.el:isearch-ring-advance.
    subroutine isearch_ring_advance(status)
        type(status_type), intent(out) :: status
        call isearch_ring_adjust(.true., status)
    end subroutine isearch_ring_advance

    !> Retreat to the previous search string in the ring.  Port of
    !> isearch.el:isearch-ring-retreat.
    subroutine isearch_ring_retreat(status)
        type(status_type), intent(out) :: status
        call isearch_ring_adjust(.false., status)
    end subroutine isearch_ring_retreat

    ! ------------------------------------------------------------------
    ! States
    ! ------------------------------------------------------------------

    !> Port of isearch.el:isearch--get-state.
    function get_state() result(s)
        type(isearch_state) :: s
        s%string = isearch_string
        s%message = isearch_message
        s%point = point()
        s%success = isearch_success
        s%forward = isearch_forward
        s%other_end = isearch_other_end
        if (allocated(isearch_error)) s%error = isearch_error
        s%wrapped = isearch_wrapped
        s%barrier = isearch_barrier
        s%case_fold = isearch_case_fold_search
    end function get_state

    !> Port of isearch.el:isearch--set-state.
    subroutine set_state(s)
        type(isearch_state), intent(in) :: s
        isearch_string = s%string
        isearch_message = s%message
        isearch_success = s%success
        isearch_forward = s%forward
        isearch_other_end = s%other_end
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (allocated(s%error)) isearch_error = s%error
        isearch_wrapped = s%wrapped
        isearch_barrier = s%barrier
        isearch_case_fold_search = s%case_fold
        call goto_char(s%point)
    end subroutine set_state

    logical function same_state(a, b)
        type(isearch_state), intent(in) :: a, b
        same_state = a%string == b%string .and. len(a%string) == len(b%string) .and. &
                     a%message == b%message .and. len(a%message) == len(b%message) .and. &
                     a%point == b%point .and. (a%success .eqv. b%success) .and. &
                     (a%forward .eqv. b%forward) .and. a%other_end == b%other_end .and. &
                     (allocated(a%error) .eqv. allocated(b%error)) .and. &
                     (a%wrapped .eqv. b%wrapped) .and. a%barrier == b%barrier .and. &
                     a%case_fold == b%case_fold
    end function same_state

    !> Port of isearch.el:isearch-push-state.
    subroutine isearch_push_state()
        type(isearch_state), allocatable :: grown(:)
        integer :: k
        if (.not. allocated(cmds)) allocate (cmds(16))
        if (ncmds == size(cmds)) then
            allocate (grown(2*size(cmds)))
            do k = 1, ncmds
                grown(k) = cmds(k)
            end do
            call move_alloc(grown, cmds)
        end if
        ncmds = ncmds + 1
        cmds(ncmds) = get_state()
    end subroutine isearch_push_state

    !> Port of isearch.el:isearch-pop-state.
    subroutine isearch_pop_state()
        ncmds = ncmds - 1
        if (ncmds > 0) call set_state(cmds(ncmds))
    end subroutine isearch_pop_state

    ! ------------------------------------------------------------------
    ! Starting and ending
    ! ------------------------------------------------------------------

    !> Do incremental search forward.
    !> With a prefix argument, do an incremental regular expression
    !> search instead (not yet: the search is literal).
    !> As you type characters, they add to the search string and are
    !> found.  Type DEL to cancel last input item from end of search
    !> string.  Type RET to exit, leaving point at location found.  Type
    !> LFD (C-j) to match end of line.  Type C-s to search again forward,
    !> C-r to search again backward.  Type C-q to quote control character
    !> to search for it.  Type M-c to toggle search case-sensitivity.
    !> Type M-e to edit the search string in the minibuffer.  Other
    !> control and meta characters terminate the search and are then
    !> executed normally.  C-g while searching or when search has failed
    !> cancels input back to what has been found successfully.  C-g when
    !> search is successful aborts and moves point to starting point.
    !> Port of isearch.el:isearch-forward.
    subroutine isearch_forward_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.true.)
    end subroutine isearch_forward_command

    !> Do incremental search backward.
    !> See the command `isearch-forward' for more information.  Port of
    !> isearch.el:isearch-backward.
    subroutine isearch_backward_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.false.)
    end subroutine isearch_backward_command

    !> Start Isearch minor mode.
    !> It is called by the function `isearch-forward' and other related
    !> functions.  The arg FORWARD means searching in the forward
    !> direction.  Port of isearch.el:isearch-mode (no REGEXP, OP-FUN,
    !> RECURSIVE-EDIT or REGEXP-FUNCTION).
    subroutine isearch_mode(forward)
        logical, intent(in) :: forward
        ! Initialize global vars.
        isearch_forward = forward
        isearch_last_case_fold_search = isearch_case_fold_search
        isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, case_fold_search)
        isearch_string = ""
        isearch_message = ""
        ncmds = 0
        isearch_success = .true.
        isearch_wrapped = .false.
        isearch_barrier = point()
        isearch_adjusted = .false.
        isearch_yank_flag = .false.
        if (allocated(isearch_error)) deallocate (isearch_error)
        isearch_other_end = 0
        isearch_just_started = .true.
        isearch_opoint = point()
        search_ring_yank_pointer = -1
        isearch_active = .true.
        ! Prevent successive calls overriding original map in
        ! `isearch-done'.  (Bug#79368)
        if (.not. same_handle(overriding_terminal_local_map, isearch_mode_map)) then
            saved_local_map = overriding_terminal_local_map
        end if
        overriding_terminal_local_map = isearch_mode_map
        saved_overriding_local_map = overriding_terminal_local_map
        call isearch_push_state()
        call isearch_update()
        if (.not. associated(pre_command_hook, isearch_pre_command_hook)) then
            saved_pre_command_hook => pre_command_hook
            pre_command_hook => isearch_pre_command_hook
        end if
    end subroutine isearch_mode

    logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> This is called after every isearch command to update the display.
    !> Port of isearch.el:isearch-update (no slow-terminal window, hscroll
    !> or highlighting).
    subroutine isearch_update()
        if (.not. input_pending_p()) call isearch_message_show()
        isearch_adjusted = .false.
        isearch_yank_flag = .false.
    end subroutine isearch_update

    !> Exit Isearch mode.
    !> Called by all commands that terminate isearch-mode.
    !> For successful search, pass no args.
    !> For a failing search, NOPUSH is t.
    !> For going to the minibuffer to edit the search string,
    !> NOPUSH is t and EDIT is t.
    !> If NOPUSH is non-nil, we don't push the string on the search ring.
    !> Port of isearch.el:isearch-done (no intangible text or hooks).
    subroutine isearch_done(nopush, edit)
        logical, intent(in), optional :: nopush, edit
        if (associated(pre_command_hook, isearch_pre_command_hook)) then
            pre_command_hook => saved_pre_command_hook
            saved_pre_command_hook => null()
        end if
        overriding_terminal_local_map = saved_local_map
        saved_local_map = null_handle()
        isearch_active = .false.
        if (len(isearch_string) > 0 .and. .not. flag(nopush)) then
            ! Update the ring data.
            call isearch_update_ring(isearch_string)
        end if
        ! If there was movement, mark the starting position.  Maybe should
        ! test difference between and set mark only if > threshold.
        if (point() /= isearch_opoint) then
            if (.not. (transient_mark_mode .and. buffers(cur)%mark_active)) then
                call push_mark(isearch_opoint, nomsg=.true.)
                if (minibuf_level == 0 .and. .not. flag(edit)) call message("Mark saved where search started")
            end if
        end if
    end subroutine isearch_done

    logical function flag(x)
        logical, intent(in), optional :: x
        flag = .false.
        if (present(x)) flag = x
    end function flag

    !> Exit search normally.
    !> However, if this is the first command after starting incremental
    !> search and `search-nonincremental-instead' is non-nil, do a
    !> nonincremental search instead via `isearch-edit-string'.  Port of
    !> isearch.el:isearch-exit.
    subroutine isearch_exit(status)
        type(status_type), intent(out) :: status
        logical :: saved
        status = status_type()
        if (search_nonincremental_instead .and. len(isearch_string) == 0) then
            saved = isearch_nonincremental
            isearch_nonincremental = .true.
            ! This calls isearch-done as well.
            call isearch_edit_string(status)
            isearch_nonincremental = saved
        else
            call isearch_done()
        end if
    end subroutine isearch_exit

    !> Terminate the search and go back to the starting point.  Port of
    !> isearch.el:isearch-cancel.
    subroutine isearch_cancel(status)
        type(status_type), intent(out) :: status
        call goto_char(isearch_opoint)
        ! Exit isearch..
        call isearch_done(nopush=.true.)
        ! ..and pass on quit signal.
        call set_error(status, FM_QUIT)
    end subroutine isearch_cancel

    !> Abort incremental search mode if searching is successful, signaling
    !> quit.  Otherwise, revert to previous successful search and continue
    !> searching.  Use `isearch-exit' to quit without signaling.  Port of
    !> isearch.el:isearch-abort.
    subroutine isearch_abort(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call discard_input()
        if (isearch_success .and. .not. allocated(isearch_error)) then
            ! If search is successful and has no incomplete regexp, move
            ! back to starting point and really do quit.
            isearch_success = .false.
            call isearch_cancel(status)
        else
            ! If search is failing, or has an incomplete regexp, rub out
            ! until it is once more successful.
            do while ((.not. isearch_success .or. allocated(isearch_error)) .and. ncmds > 1)
                call isearch_pop_state()
            end do
            call isearch_update()
        end if
    end subroutine isearch_abort

    !> Decide whether to exit Isearch mode before executing the command.
    !> Don't exit Isearch if the key sequence that invoked this command
    !> is bound in `isearch-mode-map', or if the invoked command is a
    !> prefix argument command.  Otherwise, exit Isearch (when
    !> `search-exit-option' is t) before the command is executed globally
    !> with terminated Isearch.  Port of isearch.el:isearch-pre-command-hook
    !> (isearch-allow-prefix t, no scrolling or motion).
    subroutine isearch_pre_command_hook()
        type(value_type) :: binding
        type(status_type) :: status
        ! Don't exit Isearch if we're in the middle of some
        ! `set-transient-map' thingy like `universal-argument--mode'.
        if (.not. same_handle(overriding_terminal_local_map, saved_overriding_local_map)) return
        ! Don't exit Isearch for isearch key bindings.
        if (allocated(this_command_keys)) then
            if (size(this_command_keys) > 0) then
                call lookup_key(make_keymap_ref(isearch_mode_map), this_command_keys, binding, status)
                if (status%ok() .and. .not. fixnump(binding) .and. commandp(binding)) return
            end if
        end if
        ! Handle a prefix argument.
        if (value_eq(this_command, sym("universal-argument")) .or. &
            value_eq(this_command, sym("universal-argument-more")) .or. &
            value_eq(this_command, sym("digit-argument")) .or. &
            value_eq(this_command, sym("negative-argument"))) return
        ! Other characters terminate the search and are then executed
        ! normally.
        if (search_exit_option) call isearch_done()
    end subroutine isearch_pre_command_hook

    ! ------------------------------------------------------------------
    ! Messages
    ! ------------------------------------------------------------------

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face on the failing part).
    subroutine isearch_message_show(c_q_hack)
        logical, intent(in), optional :: c_q_hack
        call message(isearch_message_string(flag(c_q_hack)))
    end subroutine isearch_message_show

    function isearch_message_string(c_q_hack) result(m)
        logical, intent(in) :: c_q_hack
        character(len=:), allocatable :: m
        m = isearch_message_prefix(isearch_nonincremental)//isearch_message// &
            isearch_message_suffix(c_q_hack)
    end function isearch_message_string

    !> The prompt before the search string ("I-search: ", "Failing
    !> wrapped I-search backward: ", ...).  Port of
    !> isearch.el:isearch-message-prefix (no lazy count, multi-buffer or
    !> input method; ELLIPSIS only matters for regexps).
    function isearch_message_prefix(nonincremental) result(m)
        logical, intent(in) :: nonincremental
        character(len=:), allocatable :: m
        integer(int32) :: c
        m = ""
        if (.not. isearch_success) m = "failing "
        if (isearch_adjusted) m = m//"pending "
        if (isearch_wrapped) then
            if (isearch_forward) then
                if (point() > isearch_opoint) m = m//"over"
            else
                if (point() < isearch_opoint) m = m//"over"
            end if
            m = m//"wrapped "
        end if
        if (.not. isearch_success) then
            ! widen-automatically is t.
            associate (b => buffers(cur))
                if (b%begv /= 1 .or. b%zv /= b%text%z) m = m//"narrowed "
            end associate
            if (isearch_case_fold_search == CASE_FOLD_NIL) m = m//"case-sensitive "
        end if
        if (nonincremental) then
            m = m//"search"
        else
            m = m//"I-search"
        end if
        if (.not. isearch_forward) m = m//" backward"
        m = m//": "
        ! Capitalize the first letter (ASCII here).
        c = iachar(m(1:1))
        if (c >= iachar("a") .and. c <= iachar("z")) m(1:1) = achar(c - 32)
    end function isearch_message_prefix

    !> Port of isearch.el:isearch-message-suffix.
    function isearch_message_suffix(c_q_hack) result(m)
        logical, intent(in) :: c_q_hack
        character(len=:), allocatable :: m
        m = ""
        if (c_q_hack) m = "^Q"
        if (allocated(isearch_error)) m = m//" ["//isearch_error//"]"
    end function isearch_message_suffix

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    !> Search for the first occurrence of STRING.  If found, move point
    !> to the end of the occurrence, update the match data, and return
    !> true.  Port of isearch.el:isearch-search-string (BOUND nil,
    !> NOERROR t; no translation-table-for-input).
    logical function isearch_search_string(string) result(found)
        character(len=*), intent(in) :: string
        integer(int64) :: pos
        type(status_type) :: status
        if (isearch_forward) then
            call search_forward(string, pos, status, noerror=SEARCH_NOERROR)
        else
            call search_backward(string, pos, status, noerror=SEARCH_NOERROR)
        end if
        found = status%ok() .and. pos > 0
    end function isearch_search_string

    !> Do the search with the current search string.  Port of
    !> isearch.el:isearch-search.
    subroutine isearch_search()
        type(status_type) :: status
        logical :: saved_fold
        if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
            isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, isearch_no_upper_case_p(isearch_string))
        end if
        saved_fold = case_fold_search
        case_fold_search = isearch_case_fold_search /= CASE_FOLD_NIL
        if (allocated(isearch_error)) deallocate (isearch_error)
        isearch_success = isearch_search_string(isearch_string)
        case_fold_search = saved_fold
        isearch_just_started = .false.
        if (isearch_success) then
            if (isearch_forward) then
                isearch_other_end = match_beginning(0, status)
            else
                isearch_other_end = match_end(0, status)
            end if
        else
            ! Ding if failed this time after succeeding last time.
            if (cmds(ncmds)%success) call ding()
            call goto_char(cmds(ncmds)%point)
        end if
    end subroutine isearch_search

    !> Whether the text at point is STRING (`looking-at` of
    !> `regexp-quote` of it), folding case with `fold`; `mend` is where the
    !> match ends.
    logical function looking_at_string(string, fold, mend) result(found)
        character(len=*), intent(in) :: string
        logical, intent(in) :: fold
        integer(int64), intent(out) :: mend
        integer(int64) :: pos, saved_pt
        type(status_type) :: status
        logical :: saved_fold
        mend = 0
        saved_pt = point()
        saved_fold = case_fold_search
        case_fold_search = fold
        call search_forward(string, pos, status, bound=min(point_max(), saved_pt + nchars(string)), &
                            noerror=SEARCH_NOERROR)
        case_fold_search = saved_fold
        found = status%ok() .and. pos > 0
        if (found) found = match_beginning(0, status) == saved_pt
        if (found) mend = pos
        call goto_char(saved_pt)
    end function looking_at_string

    !> Do the search and update the display.  Port of
    !> isearch.el:isearch-search-and-update.
    subroutine isearch_search_and_update()
        integer(int64) :: mend
        logical :: fold
        if (isearch_success) then
            ! In reverse search, adding stuff at the end may cause zero or
            ! many more chars to be matched, in the string following point.
            ! Allow all those possibilities without moving point as long as
            ! the match does not extend past search origin.
            fold = isearch_case_fold_search /= CASE_FOLD_NIL
            if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
                fold = isearch_no_upper_case_p(isearch_string)
            end if
            if (reverse_match_here()) then
                isearch_success = .true.
                if (allocated(isearch_error)) deallocate (isearch_error)
                isearch_other_end = mend
                if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
                    isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, &
                                                     isearch_no_upper_case_p(isearch_string))
                end if
            else
                ! Not regexp, not reverse, or no match at point.
                ! Do the following before moving point.
                call isearch_message_show()
                if (isearch_other_end /= 0 .and. .not. isearch_adjusted) then
                    if (isearch_forward) then
                        call goto_char(isearch_other_end)
                    else
                        call goto_char(min(isearch_opoint, isearch_barrier, isearch_other_end + 1))
                    end if
                end if
                call isearch_search()
            end if
        end if
        call isearch_push_state()
        call isearch_update()
    contains

        logical function reverse_match_here()
            reverse_match_here = .false.
            if (isearch_forward .or. isearch_adjusted) return
            if (.not. looking_at_string(isearch_string, fold, mend)) return
            reverse_match_here = isearch_yank_flag .or. mend <= min(isearch_opoint, isearch_barrier)
        end function reverse_match_here
    end subroutine isearch_search_and_update

    !> Utility for isearch-repeat-forward and isearch-repeat-backward.
    !> Port of isearch.el:isearch-repeat (isearch-wrap-pause t,
    !> isearch-repeat-on-direction-change nil).
    subroutine isearch_repeat(forward, count)
        logical, intent(in) :: forward
        integer(int64), intent(in), optional :: count
        type(value_type), allocatable :: ring(:)
        type(status_type) :: status
        integer(int64) :: n
        integer :: outer
        if (isearch_forward .eqv. forward) then
            ! C-s in forward or C-r in reverse.
            if (len(isearch_string) == 0) then
                ! If search string is empty, use last one.
                call search_ring(ring)
                if (size(ring) == 0) then
                    isearch_error = "No previous search string"
                else
                    if (stringp(ring(1))) isearch_string = xstring(ring(1))
                    isearch_message = isearch_string
                    isearch_case_fold_search = isearch_last_case_fold_search
                    ! After taking the last element, adjust ring to
                    ! previous one.
                    call isearch_ring_adjust1(.false.)
                end if
            else if (.not. isearch_success) then
                ! If already have what to search for, repeat it.
                isearch_wrapped = .true.
                if (isearch_forward) then
                    call goto_char(point_min())
                else
                    call goto_char(point_max())
                end if
            end if
        else
            ! C-s in reverse or C-r in forward, change direction.
            isearch_forward = .not. isearch_forward
            isearch_success = .true.
        end if
        ! For subsequent \| if regexp.
        isearch_barrier = point()
        if (len(isearch_string) == 0) then
            isearch_success = .true.
        else
            ! For the case when count > 1, don't keep intermediate states
            ! added to isearch-cmds by isearch-push-state in this loop.
            outer = ncmds
            n = 1
            if (present(count)) n = count
            do
                n = n - 1
                if (n < 0) exit
                if (isearch_success .and. point() == isearch_other_end .and. .not. isearch_just_started) then
                    ! If repeating a search that found an empty string,
                    ! ensure we advance.
                    if (merge(eobp(), bobp(), isearch_forward)) then
                        ! If there's nowhere to advance to, fail (and wrap
                        ! next time).
                        isearch_success = .false.
                        call ding()
                    else
                        call forward_char(status, merge(1_int64, -1_int64, isearch_forward))
                        call isearch_search()
                    end if
                else
                    call isearch_search()
                end if
                ! Update isearch-cmds, so if isearch-search fails later, it
                ! can restore old successful state from isearch-cmds.
                if (n > 0) call isearch_push_state()
                ! Stop looping on failure.
                if (.not. isearch_success .or. allocated(isearch_error)) n = 0
            end do
            ncmds = outer
        end if
        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_repeat

    !> Repeat incremental search forwards.
    !> With a numeric argument, repeat the search ARG times.
    !> A negative argument searches backwards.  Port of
    !> isearch.el:isearch-repeat-forward.
    recursive subroutine isearch_repeat_forward(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: count
        if (nilp(arg)) then
            call isearch_repeat(.true.)
            return
        end if
        count = prefix_numeric_value(arg)
        if (count < 0) then
            call isearch_repeat_backward(make_count(abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.true.)
        else
            ! Take into account one iteration to reverse direction.
            if (.not. isearch_forward) count = count + 1
            call isearch_repeat(.true., count)
        end if
    end subroutine isearch_repeat_forward

    !> Repeat incremental search backwards.
    !> With a numeric argument, repeat the search ARG times.
    !> A negative argument searches forwards.  Port of
    !> isearch.el:isearch-repeat-backward.
    recursive subroutine isearch_repeat_backward(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: count
        if (nilp(arg)) then
            call isearch_repeat(.false.)
            return
        end if
        count = prefix_numeric_value(arg)
        if (count < 0) then
            call isearch_repeat_forward(make_count(abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.false.)
        else
            ! Take into account one iteration to reverse direction.
            if (isearch_forward) count = count + 1
            call isearch_repeat(.false., count)
        end if
    end subroutine isearch_repeat_backward

    function make_count(n) result(v)
        integer(int64), intent(in) :: n
        type(value_type) :: v
        v = make_fixnum(n)
    end function make_count

    !> Go to the first occurrence of the current search string.
    !> Move point to the beginning of the buffer and search forwards from
    !> the top.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the beginning of the buffer.  Port of
    !> isearch.el:isearch-beginning-of-buffer.
    recursive subroutine isearch_beginning_of_buffer(arg)
        integer(int64), intent(in) :: arg
        logical :: current_forward
        if (arg < 0) then
            call isearch_end_of_buffer(abs(arg))
            return
        end if
        ! For the case when the match is at bobp, don't forward char in
        ! isearch-repeat.
        isearch_just_started = .true.
        call goto_char(point_min())
        current_forward = isearch_forward
        call isearch_repeat(.true., arg)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_beginning_of_buffer

    !> Go to the last occurrence of the current search string.
    !> Move point to the end of the buffer and search backwards from the
    !> bottom.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the end of the buffer.  Port of
    !> isearch.el:isearch-end-of-buffer.
    recursive subroutine isearch_end_of_buffer(arg)
        integer(int64), intent(in) :: arg
        logical :: current_forward
        if (arg < 0) then
            call isearch_beginning_of_buffer(abs(arg))
            return
        end if
        isearch_just_started = .true.
        call goto_char(point_max())
        current_forward = isearch_forward
        call isearch_repeat(.false., arg)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_end_of_buffer

    ! ------------------------------------------------------------------
    ! Editing the search string
    ! ------------------------------------------------------------------

    !> Undo last input item during a search.
    !> An input item is the result of a command that pushes a new state
    !> of isearch to `isearch-cmds'.  If no input items have been entered
    !> yet, just beep.  Port of isearch.el:isearch-delete-char.
    subroutine isearch_delete_char()
        if (ncmds <= 1) then
            call ding()
        else
            call isearch_pop_state()
        end if
        call isearch_update()
    end subroutine isearch_delete_char

    !> Delete character from end of search string and search again.
    !> Unlike `isearch-delete-char', it only deletes the last character,
    !> but doesn't cancel the effect of other isearch command.
    !> If search string is empty, just beep.  Port of
    !> isearch.el:isearch-del-char.
    subroutine isearch_del_char(arg)
        integer(int64), intent(in) :: arg
        if (len(isearch_string) == 0) then
            call ding()
        else
            isearch_string = drop_chars(isearch_string, int(min(arg, int(nchars(isearch_string), int64))))
            isearch_message = isearch_string
        end if
        ! Do the following before moving point.
        call isearch_message_show()
        ! Use the isearch-other-end as new starting point to be able to
        ! find the remaining part of the search string again.
        if (isearch_other_end /= 0) call goto_char(isearch_other_end)
        call isearch_search()
        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_del_char

    !> Add this ordinary printing CHAR to the search string and search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-printing-char.
    subroutine isearch_printing_char(char, count)
        integer(int64), intent(in) :: char, count
        integer(int64) :: c
        c = char
        if (c == ior(int(CHAR_SHIFT, int64), 32_int64)) c = 32
        call isearch_process_search_char(c, count)
    end subroutine isearch_printing_char

    !> Add CHAR to the search string, COUNT times.
    !> Search is updated accordingly.  Port of
    !> isearch.el:isearch-process-search-char.
    subroutine isearch_process_search_char(char, count)
        integer(int64), intent(in) :: char, count
        character(len=:), allocatable :: one, string
        one = encode(int(char, int32))
        string = one
        if (count > 1) string = repeat(one, int(count))
        ! Append the char(s) to the search string, update the message and
        ! re-search.
        call isearch_process_search_string(string, string)
    end subroutine isearch_process_search_char

    !> Port of isearch.el:isearch-process-search-string.
    subroutine isearch_process_search_string(string, message)
        character(len=*), intent(in) :: string, message
        isearch_string = isearch_string//string
        isearch_message = isearch_message//message
        call isearch_search_and_update()
    end subroutine isearch_process_search_string

    !> Pull STRING into search string.  Port of
    !> isearch.el:isearch-yank-string.
    subroutine isearch_yank_string(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: s
        s = string
        ! Downcase the string if not supposed to case-fold yanked strings.
        if (isearch_case_fold_search /= CASE_FOLD_NIL .and. search_upper_case == UPPER_CASE_NOT_YANKS) then
            s = downcase_string(s)
        end if
        ! Don't move cursor in reverse search.
        isearch_yank_flag = .true.
        call isearch_process_search_string(s, s)
    end subroutine isearch_yank_string

    !> Pull the text from point to the point reached by `jump`: 1 moves
    !> forward `arg` characters, 2 to the end of the line (`arg` lines
    !> on).  Port of isearch.el:isearch-yank-internal.
    subroutine isearch_yank_internal(jump, arg)
        integer, intent(in) :: jump
        integer(int64), intent(in) :: arg
        character(len=:), allocatable :: text
        type(status_type) :: status
        integer(int64) :: saved, from
        saved = point()
        if (.not. isearch_forward .and. isearch_other_end /= 0) call goto_char(isearch_other_end)
        if (.not. isearch_success .and. isearch_yank_prev_point /= 0) call goto_char(isearch_yank_prev_point)
        from = point()
        if (jump == 1) then
            call forward_char(status, arg)
        else
            call goto_char(pos_eol(merge(arg + 1, arg, eolp())))
        end if
        isearch_yank_prev_point = point()
        if (isearch_success) isearch_yank_prev_point = 0
        call buffer_substring(from, point(), text, status)
        call goto_char(saved)
        call isearch_yank_string(text)
    end subroutine isearch_yank_internal

    !> Pull next character from buffer into search string.
    !> If optional ARG is non-nil, pull in the next ARG characters.  Port
    !> of isearch.el:isearch-yank-char.
    subroutine isearch_yank_char(arg)
        integer(int64), intent(in) :: arg
        call isearch_yank_internal(1, arg)
    end subroutine isearch_yank_char

    !> Pull rest of line from buffer into search string.
    !> If optional ARG is non-nil, yank the next ARG lines.  Port of
    !> isearch.el:isearch-yank-line.
    subroutine isearch_yank_line(arg)
        integer(int64), intent(in) :: arg
        call isearch_yank_internal(2, arg)
    end subroutine isearch_yank_line

    !> Pull string from kill ring into search string.  Port of
    !> isearch.el:isearch-yank-kill.
    subroutine isearch_yank_kill(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        if (.not. isearch_active) call isearch_mode(.true.)
        call current_kill(0_int64, text, status)
        if (status%ok()) call isearch_yank_string(text)
    end subroutine isearch_yank_kill

    !> Replace just-yanked search string with previously killed string.
    !> When this command is called not immediately after a
    !> `isearch-yank-kill' or a `isearch-yank-pop-only', it only pops the
    !> last killed string.  Port of isearch.el:isearch-yank-pop-only (no
    !> C-u: reading from the kill ring needs the minibuffer completion).
    subroutine isearch_yank_pop_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        if (.not. (value_eq(last_command, sym("isearch-yank-kill")) .or. &
                   value_eq(last_command, sym("isearch-yank-pop")) .or. &
                   value_eq(last_command, sym("isearch-yank-pop-only")))) then
            call isearch_yank_kill(status)
        else
            call isearch_pop_state()
            call current_kill(1_int64, text, status)
            if (status%ok()) call isearch_yank_string(text)
        end if
    end subroutine isearch_yank_pop_only

    !> Toggle case folding in searching on or off.
    !> Toggles the value of the variable `isearch-case-fold-search'.
    !> Port of isearch.el:isearch-toggle-case-fold.
    subroutine isearch_toggle_case_fold()
        if (.not. isearch_active) call isearch_mode(.true.)
        if (isearch_case_fold_search /= CASE_FOLD_NIL) then
            isearch_case_fold_search = CASE_FOLD_NIL
            call message("case sensitive")
        else
            isearch_case_fold_search = CASE_FOLD_YES
            call message("case insensitive")
        end if
        isearch_success = .true.
        isearch_adjusted = .true.
        call isearch_update()
    end subroutine isearch_toggle_case_fold

    !> Edit the search string in the minibuffer.
    !> RET resumes incremental searching with the edited string.  Port of
    !> isearch.el:isearch-edit-string with with-isearch-suspended (no
    !> minibuffer-local-isearch-map).
    recursive subroutine isearch_edit_string(status)
        type(status_type), intent(out) :: status
        type(isearch_state), allocatable :: saved_cmds(:)
        type(value_type), allocatable :: ring(:)
        character(len=:), allocatable :: new_string, new_message, saved_error, input
        logical :: new_forward, new_nonincremental, saved_success, saved_wrapped, saved_adjusted, &
                   saved_yank_flag, had_error, saved_history_add
        integer :: new_case_fold, saved_ncmds
        integer(int64) :: saved_barrier, saved_opoint, old_point, old_other_end
        type(isearch_state) :: now
        status = status_type()
        new_nonincremental = isearch_nonincremental
        ! Locally bind all isearch global variables to protect them from
        ! recursive isearching.
        new_string = isearch_string
        new_message = isearch_message
        new_forward = isearch_forward
        new_case_fold = isearch_case_fold_search
        saved_ncmds = ncmds
        if (ncmds > 0) saved_cmds = cmds(1:ncmds)
        saved_success = isearch_success
        saved_wrapped = isearch_wrapped
        saved_barrier = isearch_barrier
        saved_adjusted = isearch_adjusted
        saved_yank_flag = isearch_yank_flag
        had_error = allocated(isearch_error)
        if (had_error) saved_error = isearch_error
        saved_opoint = isearch_opoint
        ! Actually terminate isearching until editing is done.
        call isearch_done(nopush=.true., edit=.true.)
        ! Save old point and isearch-other-end before reading from
        ! minibuffer that can change their values.
        old_point = point()
        old_other_end = isearch_other_end
        ! Don't add a new search string to the search ring here; it should
        ! be added only by `isearch-update-ring' called from `isearch-done'.
        saved_history_add = history_add_new_input
        history_add_new_input = .false.
        call read_from_minibuffer(isearch_message_prefix(isearch_nonincremental), input, status, &
                                  initial=isearch_string, history="search-ring")
        history_add_new_input = saved_history_add
        if (status%ok()) then
            new_string = input
            new_message = input
        end if
        ! Always resume isearching by restarting it.
        if (.not. isearch_active) call isearch_mode(isearch_forward)
        ! Copy new local values to isearch globals.
        isearch_string = new_string
        isearch_message = new_message
        isearch_forward = new_forward
        isearch_nonincremental = new_nonincremental
        isearch_case_fold_search = new_case_fold
        ! Restore the minibuffer message before moving point.
        call isearch_message_show()
        ! Set point at the start (end) of old match if forward (backward),
        ! so after exiting minibuffer isearch resumes at the start (end) of
        ! this match and can find it again.
        if (old_other_end /= 0 .and. old_point == point()) call goto_char(old_other_end)
        if (status%ok()) then
            ! Empty isearch-string means use default.
            if (len(isearch_string) == 0) then
                call search_ring(ring)
                isearch_string = ""
                if (size(ring) > 0) isearch_string = xstring(ring(1))
                isearch_message = isearch_string
                ! After taking the last element, adjust ring to previous
                ! one.
                call isearch_ring_adjust1(.false.)
            end if
            ! Reinvoke the pending search.
            call isearch_search()
            ! If no code has changed the search parameters, then pushing a
            ! new state of Isearch should not be necessary.
            now = get_state()
            if (ncmds == 0) then
                call isearch_push_state()
            else if (.not. same_state(cmds(ncmds), now)) then
                call isearch_push_state()
            end if
            call isearch_update()
            if (isearch_nonincremental) then
                call isearch_done()
                ! The search done message is confusing when the string is
                ! empty, so erase it.
                if (len(isearch_string) == 0) call message("")
            end if
        end if
        ! The let's bindings end.
        ncmds = saved_ncmds
        if (saved_ncmds > 0) cmds(1:saved_ncmds) = saved_cmds
        isearch_success = saved_success
        isearch_wrapped = saved_wrapped
        isearch_barrier = saved_barrier
        isearch_adjusted = saved_adjusted
        isearch_yank_flag = saved_yank_flag
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (had_error) isearch_error = saved_error
        isearch_opoint = saved_opoint
        ! Handle abort-recursive-edit.
        if (status%code == FM_QUIT) call isearch_abort(status)
    end subroutine isearch_edit_string

    !> Quote special characters for incremental search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-quote-char.
    subroutine isearch_quote_char(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have
        integer(int64) :: c
        call message(isearch_message_string(.true.))
        call read_event(ev, have, status)
        if (.not. status%ok() .or. .not. have) return
        c = -1
        if (fixnump(ev)) c = xfixnum(ev)
        if (c < 0 .or. c > MAX_CHAR) then
            call set_error(status, FM_USER_ERROR, "Not a valid character")
            return
        end if
        call isearch_process_search_char(c, count)
    end subroutine isearch_quote_char

    ! ------------------------------------------------------------------
    ! Commands and the keymap
    ! ------------------------------------------------------------------

    integer(int64) function arg_p()
        arg_p = prefix_numeric_value(current_prefix_arg)
    end function arg_p

    subroutine cmd_isearch_exit(status)
        type(status_type), intent(out) :: status
        call isearch_exit(status)
    end subroutine cmd_isearch_exit

    subroutine cmd_isearch_cancel(status)
        type(status_type), intent(out) :: status
        call isearch_cancel(status)
    end subroutine cmd_isearch_cancel

    subroutine cmd_isearch_abort(status)
        type(status_type), intent(out) :: status
        call isearch_abort(status)
    end subroutine cmd_isearch_abort

    subroutine cmd_isearch_repeat_forward(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_repeat_forward(current_prefix_arg)
    end subroutine cmd_isearch_repeat_forward

    subroutine cmd_isearch_repeat_backward(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_repeat_backward(current_prefix_arg)
    end subroutine cmd_isearch_repeat_backward

    subroutine cmd_isearch_delete_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_delete_char()
    end subroutine cmd_isearch_delete_char

    subroutine cmd_isearch_del_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_del_char(arg_p())
    end subroutine cmd_isearch_del_char

    subroutine cmd_isearch_printing_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. fixnump(last_command_event)) return
        call isearch_printing_char(xfixnum(last_command_event), arg_p())
    end subroutine cmd_isearch_printing_char

    subroutine cmd_isearch_quote_char(status)
        type(status_type), intent(out) :: status
        call isearch_quote_char(arg_p(), status)
    end subroutine cmd_isearch_quote_char

    subroutine cmd_isearch_yank_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_yank_char(arg_p())
    end subroutine cmd_isearch_yank_char

    subroutine cmd_isearch_yank_line(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_yank_line(arg_p())
    end subroutine cmd_isearch_yank_line

    subroutine cmd_isearch_beginning_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_beginning_of_buffer(arg_p())
    end subroutine cmd_isearch_beginning_of_buffer

    subroutine cmd_isearch_end_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_end_of_buffer(arg_p())
    end subroutine cmd_isearch_end_of_buffer

    subroutine cmd_isearch_yank_kill(status)
        type(status_type), intent(out) :: status
        call isearch_yank_kill(status)
    end subroutine cmd_isearch_yank_kill

    subroutine cmd_isearch_yank_pop_only(status)
        type(status_type), intent(out) :: status
        call isearch_yank_pop_only(status)
    end subroutine cmd_isearch_yank_pop_only

    subroutine cmd_isearch_toggle_case_fold(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_toggle_case_fold()
    end subroutine cmd_isearch_toggle_case_fold

    subroutine cmd_isearch_edit_string(status)
        type(status_type), intent(out) :: status
        call isearch_edit_string(status)
    end subroutine cmd_isearch_edit_string

    subroutine cmd_isearch_ring_advance(status)
        type(status_type), intent(out) :: status
        call isearch_ring_advance(status)
    end subroutine cmd_isearch_ring_advance

    subroutine cmd_isearch_ring_retreat(status)
        type(status_type), intent(out) :: status
        call isearch_ring_retreat(status)
    end subroutine cmd_isearch_ring_retreat

    !> Bind `keys` (kbd syntax) to command `cmd` in `map`.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    !> Register the isearch commands, build `isearch-mode-map`, and bind
    !> C-s, C-r, C-M-s and C-M-r in `global_map`.  Port of isearch.el's
    !> isearch-mode-map and global bindings.
    subroutine syms_of_isearch(global_map)
        type(handle_type), intent(in) :: global_map
        type(handle_type) :: meta_map
        type(value_type) :: key(1)
        type(status_type) :: status
        call defcommand("isearch-forward", isearch_forward_command)
        call defcommand("isearch-backward", isearch_backward_command)
        call defcommand("isearch-exit", cmd_isearch_exit)
        call defcommand("isearch-cancel", cmd_isearch_cancel)
        call defcommand("isearch-abort", cmd_isearch_abort)
        call defcommand("isearch-repeat-forward", cmd_isearch_repeat_forward)
        call defcommand("isearch-repeat-backward", cmd_isearch_repeat_backward)
        call defcommand("isearch-delete-char", cmd_isearch_delete_char)
        call defcommand("isearch-del-char", cmd_isearch_del_char)
        call defcommand("isearch-printing-char", cmd_isearch_printing_char)
        call defcommand("isearch-quote-char", cmd_isearch_quote_char)
        call defcommand("isearch-yank-char", cmd_isearch_yank_char)
        call defcommand("isearch-yank-line", cmd_isearch_yank_line)
        call defcommand("isearch-beginning-of-buffer", cmd_isearch_beginning_of_buffer)
        call defcommand("isearch-end-of-buffer", cmd_isearch_end_of_buffer)
        call defcommand("isearch-toggle-case-fold", cmd_isearch_toggle_case_fold)
        call defcommand("isearch-edit-string", cmd_isearch_edit_string)
        call defcommand("isearch-ring-advance", cmd_isearch_ring_advance)
        call defcommand("isearch-ring-retreat", cmd_isearch_ring_retreat)
        call defcommand("isearch-yank-kill", cmd_isearch_yank_kill)
        call defcommand("isearch-yank-pop-only", cmd_isearch_yank_pop_only)
        if (keymap_live_p(isearch_mode_map)) call free_keymap(isearch_mode_map, status)
        call make_keymap(isearch_mode_map)
        ! Make all multibyte characters search for themselves.
        call define_key_range(isearch_mode_map, 256_int64, int(MAX_CHAR, int64), &
                              sym("isearch-printing-char"), status)
        ! Single-byte printing chars extend the search string by default.
        call define_key_range(isearch_mode_map, 32_int64, 255_int64, sym("isearch-printing-char"), status)
        ! To handle local bindings with meta char prefix keys, define
        ! another full keymap.
        call make_sparse_keymap(meta_map)
        key(1) = make_count(27_int64)
        call define_key(isearch_mode_map, key, make_keymap_ref(meta_map), status)
        ! Several non-printing chars change the searching behavior.
        call bind(isearch_mode_map, "C-s", "isearch-repeat-forward")
        call bind(isearch_mode_map, "C-r", "isearch-repeat-backward")
        ! Define M-C-s and M-C-r like C-s and C-r so that the same key
        ! combinations can be used to repeat regexp isearches that can be
        ! used to start these searches.
        call bind(isearch_mode_map, "C-M-s", "isearch-repeat-forward")
        call bind(isearch_mode_map, "C-M-r", "isearch-repeat-backward")
        call bind(isearch_mode_map, "DEL", "isearch-delete-char")
        call bind(isearch_mode_map, "<backspace>", "undefined")
        call bind(isearch_mode_map, "C-g", "isearch-abort")
        call bind(isearch_mode_map, "ESC ESC ESC", "isearch-cancel")
        call bind(isearch_mode_map, "C-q", "isearch-quote-char")
        call bind(isearch_mode_map, "RET", "isearch-exit")
        call bind(isearch_mode_map, "<return>", "isearch-exit")
        call bind(isearch_mode_map, "C-j", "isearch-printing-char")
        call bind(isearch_mode_map, "TAB", "isearch-printing-char")
        call bind(isearch_mode_map, "S-SPC", "isearch-printing-char")
        call bind(isearch_mode_map, "C-M-d", "isearch-del-char")
        call bind(isearch_mode_map, "C-M-y", "isearch-yank-char")
        call bind(isearch_mode_map, "M-s C-e", "isearch-yank-line")
        call bind(isearch_mode_map, "M-s M-<", "isearch-beginning-of-buffer")
        call bind(isearch_mode_map, "M-s M->", "isearch-end-of-buffer")
        call bind(isearch_mode_map, "M-n", "isearch-ring-advance")
        call bind(isearch_mode_map, "M-p", "isearch-ring-retreat")
        call bind(isearch_mode_map, "C-y", "isearch-yank-kill")
        call bind(isearch_mode_map, "M-y", "isearch-yank-pop-only")
        call bind(isearch_mode_map, "M-c", "isearch-toggle-case-fold")
        call bind(isearch_mode_map, "M-s c", "isearch-toggle-case-fold")
        call bind(isearch_mode_map, "M-e", "isearch-edit-string")
        call bind(isearch_mode_map, "M-s e", "isearch-edit-string")
        call bind(global_map, "C-s", "isearch-forward")
        call bind(global_map, "C-r", "isearch-backward")
    end subroutine syms_of_isearch
end module isearch_fm
