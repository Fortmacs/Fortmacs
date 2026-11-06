! follow_mod.f90 -- synchronize windows showing the same buffer.
! Ported from GNU Emacs 31.1 lisp/follow.el.
!
! Copyright (C) 1995-2026 Free Software Foundation, Inc.
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

!> Follow mode: the windows of a frame that show the same buffer act as
!> one tall window.  Before each redisplay (`pre_redisplay_function') the
!> windows of the selected window's chain are aligned, each starting
!> where the one before it ends, and a window showing point is selected.
!> A window chain is ordered by the windows' top edges, then their left
!> edges.  What is not ported yet is in docs/status/follow_mod.md.
module follow_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, END_OF_BUFFER_CODE, BEGINNING_OF_BUFFER_CODE
    use handle_mod, only: handle_type, null_handle
    use hooks_mod, only: hook_type
    use value_mod, only: make_keymap_reference
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use execute_interactively_mod, only: prefix_argument_type, current_prefix_argument, prefix_numeric_value, this_command, &
                                         PREFIX_NONE, PREFIX_NUMBER, PREFIX_MINUS
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_list, resolve_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_buffer, &
                                     is_end_of_buffer, is_beginning_of_line
    use indent_mod, only: vertical_motion, current_column
    use keymap_mod, only: make_sparse_keymap, keymap_set, define_key, key_parse
    use keyboard_mod, only: is_input_pending, sit_for
    use display_update_mod, only: redisplay
    use display_engine_mod, only: message, pre_redisplay_function, pre_redisplay_procedure
    use frame_mod, only: selected_frame, selected_window
    use window_mod, only: windows, window_start, set_window_start, window_point, set_window_point, window_end, &
                          is_window_live, window_list, window_list_1, next_window, select_window, same_window, &
                          window_body_lines, is_position_visible_in_window, recenter, scroll_up_command, scroll_down_command, &
                          delete_other_windows, split_window, other_window, switch_to_buffer, window_leaves, &
                          window_scroll_functions, scroll_error_top_bottom, next_screen_context_lines, other_buffer, &
                          MINIBUF_NO
    use simple_mod, only: end_of_buffer, is_region_active
    use minibuffer_mod, only: read_buffer
    use help_mod, only: register_keymap_name
    implicit none(type, external)
    private

    public :: follow_mode, turn_on_follow_mode, turn_off_follow_mode, scroll_follow_up_window, &
              scroll_follow_down_window, scroll_follow_up, scroll_follow_down, follow_delete_other_windows_and_split, &
              follow_switch_to_buffer, follow_switch_to_buffer_all, follow_switch_to_current_buffer_all, &
              next_follow_window, previous_follow_window, follow_first_window, follow_last_window, recenter_follow, &
              redraw_follow, follow_end_of_buffer, follow_all_followers, redisplay_follow, follow_post_command_hook, &
              adjust_follow_window, avoid_follow_tail_recenter, adjust_follow_windows, initialize_follow

    !> Normal hook run by `follow_mode'.  Port of follow.el follow-mode-hook.
    type(hook_type), target, public :: follow_mode_hook = hook_type(name="follow_mode_hook")

    !> Prefix key to use for follow commands in Follow mode.  (The value is
    !> used when Follow mode's keymap is made, at startup.)  Port of
    !> follow.el follow-mode-prefix-key.
    character(len=32), public :: follow_mode_prefix_key = "C-c ."

    !> Text shown in the mode line when Follow mode is active.
    !> Defaults to " Follow".  Examples of other values
    !> are " Fw", or simply "".  (The value is used when Follow mode is
    !> defined, at startup.)  Port of follow.el follow-mode-line-text.
    character(len=32), public :: follow_mode_line_text = " Follow"

    !> When true, Follow mode attempts to hide the obtrusive cursors
    !> in the non-selected windows of a window group.
    !> This variable takes effect when `follow_mode' is initialized.
    !> Due to limitations in Emacs, this only operates on the followers
    !> of the selected window.  Port of follow.el follow-hide-ghost-cursors
    !> (a text terminal shows only the selected window's cursor).
    logical, public :: follow_hide_ghost_cursors = .true.

    !> If true, emit Follow mode debugging messages.  Port of follow.el
    !> follow-debug.
    logical, public :: follow_debug = .false.

    !> True when Follow mode should redisplay the windows.  Port of
    !> follow.el follow-internal-force-redisplay.
    logical, public :: follow_internal_force_redisplay = .false.

    !> True when inside Follow modes `post_command_hook'.  Port of
    !> follow.el follow-inside-post-command-hook-call.
    logical, public :: follow_inside_post_command_hook_call = .false.

    !> If true, the current window must not be scrolled.
    !> This is typically set by explicit scrolling commands.  Port of
    !> follow.el follow-fixed-window.
    logical, public :: follow_fixed_window = .false.

    !> What `follow_windows_start_end' tells about one window of a chain
    !> (upstream's (WINDOW START END BUFFER-END-P)): where it starts, where its
    !> display ends, and whether the end of the buffer shows in it.
    type :: window_start_end_type
        type(handle_type) :: window
        integer(int64) :: start = 0, end = 0
        logical :: buffer_end = .false.
    end type window_start_end_type

    !> Cache used by `follow_window_start' and `follow_window_end'
    !> (unallocated for none).  Port of follow.el
    !> follow-windows-start-end-cache.
    type(window_start_end_type), allocatable :: follow_windows_start_end_cache(:)

    !> Minor mode keymap for Follow mode.  Port of follow.el follow-mode-map.
    type(handle_type), public :: follow_mode_map

    !> The keymap of the Follow mode commands, on `follow_mode_prefix_key'
    !> in `follow_mode_map'.  Port of follow.el follow-mode-submap.
    type(handle_type), public :: follow_mode_submap

    !> The `pre_redisplay_function' Follow mode goes before (add-function
    !> :before), and whether it does.
    procedure(pre_redisplay_procedure), pointer :: original_pre_redisplay_function => null()
    logical :: advised = .false.

contains

    !> Like `message', but only active when `follow_debug' is true.  Port of
    !> follow.el:follow-debug-message.
    subroutine follow_debug_message(text)
        character(len=*), intent(in) :: text

        if (follow_debug) call message(text)
    end subroutine follow_debug_message

    !> Turn on Follow mode.  Please see the function `follow_mode'.  Port of
    !> follow.el:turn-on-follow-mode.
    recursive subroutine turn_on_follow_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%follow_mode%enable(status)
    end subroutine turn_on_follow_mode

    !> Turn off Follow mode.  Please see the function `follow_mode'.  Port of
    !> follow.el:turn-off-follow-mode.
    recursive subroutine turn_off_follow_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%follow_mode%disable(status)
    end subroutine turn_off_follow_mode

    !> Toggle Follow mode.
    !>
    !> Follow mode is a minor mode that combines windows into one tall
    !> virtual window.  This is accomplished by two main techniques:
    !>
    !> * The windows always displays adjacent sections of the buffer.
    !>   This means that whenever one window is moved, all the
    !>   others will follow.  (Hence the name Follow mode.)
    !>
    !> * Should point (cursor) end up outside a window, another
    !>   window displaying that point is selected, if possible.  This
    !>   makes it possible to walk between windows using normal cursor
    !>   movement commands.
    !>
    !> Follow mode comes to its prime when used on a large screen and two or
    !> more side-by-side windows are used.  The user can, with the help of
    !> Follow mode, use these full-height windows as though they were one.
    !> Imagine yourself editing a large function, or section of text, and
    !> being able to use 144 or 216 lines instead of the normal 72... (your
    !> mileage may vary).
    !>
    !> To split one large window into two side-by-side windows, the commands
    !> \[split_window_right] or \[follow_delete_other_windows_and_split] can
    !> be used.
    !>
    !> Only windows displayed in the same frame follow each other.
    !>
    !> This command runs the normal hook `follow_mode_hook'.
    !>
    !> Keys specific to Follow mode:
    !> \{follow_mode_map}
    !> Port of follow.el:follow-mode.
    recursive subroutine follow_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%follow_mode%command(status)
    end subroutine follow_mode

    !> The body of Follow mode.  Port of follow.el:follow-mode in part.
    recursive subroutine follow_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        integer(int64) :: bi
        type(status_type) :: st
        logical :: following
        integer :: k

        status = status_type()

        if (enable) then
            if (.not. advised) then
                original_pre_redisplay_function => pre_redisplay_function
                pre_redisplay_function => adjust_follow_windows_then_redisplay
                advised = .true.
            end if

            return
        end if

        ! Remove globally-installed hook functions only if there is no
        ! other Follow mode buffer.
        list = buffer_list()
        following = .false.

        do k = 1, size(list)
            call resolve_buffer(list(k), bi, st)
            if (.not. st%ok()) cycle

            if (minor_modes%follow_mode%enabled_in(bi)) then
                following = .true.
                exit
            end if
        end do

        if (.not. following .and. advised) then
            pre_redisplay_function => original_pre_redisplay_function
            original_pre_redisplay_function => null()
            advised = .false.
        end if
    end subroutine follow_mode_body

    !> Follow mode's function before the original `pre_redisplay_function'
    !> (add-function :before).  Not a port.
    recursive subroutine adjust_follow_windows_then_redisplay(all, windows)
        logical, intent(in) :: all
        type(handle_type), intent(in) :: windows(:)

        call adjust_follow_windows(all, windows)
        if (associated(original_pre_redisplay_function)) call original_pre_redisplay_function(all, windows)
    end subroutine adjust_follow_windows_then_redisplay

    ! ------------------------------------------------------------------
    ! Windows: points, starts and the chain
    ! ------------------------------------------------------------------

    !> The point of WINDOW: its buffer's point when it is the selected
    !> window.  Port of window.c:Fwindow_point.
    integer(int64) function point_of(window) result(position)
        type(handle_type), intent(in) :: window
        integer(int64) :: bi
        type(status_type) :: st

        position = window_point(windows(window%index))

        if (same_window(window, selected_window())) then
            call resolve_buffer(windows(window%index)%buffer, bi, st)
            if (st%ok()) position = buffers(bi)%point
        end if
    end function point_of

    !> Make point value in WINDOW be at position POSITION in WINDOW's buffer.
    !> Port of window.c:Fset_window_point.
    subroutine set_point_of(window, position)
        type(handle_type), intent(in) :: window
        integer(int64), intent(in) :: position
        type(handle_type) :: old
        type(status_type) :: st

        if (same_window(window, selected_window())) then
            old = current_buffer()
            call set_buffer(windows(window%index)%buffer, st)
            if (.not. st%ok()) return
            call goto_character(position)
            if (is_buffer_live(old)) call set_buffer(old, st)
        else
            call set_window_point(windows(window%index), position, st)
        end if
    end subroutine set_point_of

    !> Where the display of WINDOW starts.  Port of window.c:Fwindow_start.
    integer(int64) function start_of(window) result(position)
        type(handle_type), intent(in) :: window

        position = window_start(windows(window%index))
    end function start_of

    !> Make the display of WINDOW start at POSITION; with NOFORCE, without
    !> forcing the next redisplay to keep it (and leaving a start forced
    !> before as it was).  Port of window.c:Fset_window_start.
    subroutine set_start_of(window, position, noforce)
        type(handle_type), intent(in) :: window
        integer(int64), intent(in) :: position
        logical, intent(in), optional :: noforce
        type(status_type) :: st
        logical :: forced

        forced = windows(window%index)%force_start
        call set_window_start(windows(window%index), position, st)

        if (present(noforce)) then
            if (noforce) windows(window%index)%force_start = forced
        end if
    end subroutine set_start_of

    !> Whether WINDOW shows buffer slot BI.  Not a port.
    logical function shows_buffer(window, bi) result(shows)
        type(handle_type), intent(in) :: window
        integer(int64), intent(in) :: bi
        integer(int64) :: b
        type(status_type) :: st

        call resolve_buffer(windows(window%index)%buffer, b, st)
        shows = st%ok() .and. b == bi
    end function shows_buffer

    !> Select WINDOW, without recording it as recently used with NORECORD.
    !> Port of window.c:Fselect_window.
    subroutine select_live_window(window, norecord)
        type(handle_type), intent(in) :: window
        logical, intent(in), optional :: norecord
        type(status_type) :: st

        if (.not. is_window_live(window)) return
        call select_window(window, st, norecord)
    end subroutine select_live_window

    !> Whether WINDOW1 is above WINDOW2, or at the same height and to its
    !> left.  Port of follow.el:follow--window-sorter.
    logical function is_window_before(window1, window2) result(before)
        type(handle_type), intent(in) :: window1, window2

        associate (w1 => windows(window1%index), w2 => windows(window2%index))
            if (w1%top_line == w2%top_line) then
                before = w1%left_column < w2%left_column
            else
                before = w1%top_line < w2%top_line
            end if
        end associate
    end function is_window_before

    !> Return all windows displaying the same buffer as the WINDOW.
    !> The list is sorted with topmost and leftmost windows first, and
    !> contains only windows in the same frame as WINDOW.  If WINDOW is absent,
    !> it defaults to the selected window.  Port of
    !> follow.el:follow-all-followers.
    function follow_all_followers(window) result(followers)
        type(handle_type), intent(in), optional :: window
        type(handle_type), allocatable :: followers(:)
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: w, x
        type(status_type) :: st
        integer(int64) :: bi
        integer :: k, j, n

        w = selected_window()

        if (present(window)) then
            if (is_window_live(window)) w = window
        end if

        call resolve_buffer(windows(w%index)%buffer, bi, st)
        ! get-buffer-window-list: the windows showing it, but the
        ! minibuffer window.
        list = window_list(MINIBUF_NO, w, st)
        allocate (followers(size(list)))
        n = 0

        do k = 1, size(list)
            if (.not. shows_buffer(list(k), bi)) cycle
            n = n + 1
            followers(n) = list(k)
        end do

        followers = followers(1:n)

        ! Sort them (an insertion sort: stable, as `sort').
        do k = 2, n
            x = followers(k)
            j = k - 1

            do while (j >= 1)
                if (.not. is_window_before(x, followers(j))) exit
                followers(j + 1) = followers(j)
                j = j - 1
            end do

            followers(j + 1) = x
        end do
    end function follow_all_followers

    !> Split WINDOWS into two sets: predecessors and successors.
    !> TEST and SUCC are ordered starting from the window WINDOW (by default
    !> the selected window).  Port of follow.el:follow-split-followers.
    subroutine split_follow_followers(list, test, succ, window)
        type(handle_type), intent(in) :: list(:)
        type(handle_type), allocatable, intent(out) :: test(:), succ(:)
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w
        integer :: k

        w = selected_window()
        if (present(window)) w = window
        k = 1

        do while (k <= size(list))
            if (same_window(list(k), w)) exit
            k = k + 1
        end do

        test = list(k - 1:1:-1)

        if (k <= size(list)) then
            succ = list(k + 1:)
        else
            allocate (succ(0))
        end if
    end subroutine split_follow_followers

    !> Calculate the end position for window WINDOW: END_POSITION and END_OF_BUFFER.
    !> Actually, the position returned is the start of the line after
    !> the last fully-visible line in WINDOW.  END_OF_BUFFER is true when END_OF_BUFFER
    !> is fully-visible in WINDOW.  If WINDOW is absent, the selected window is
    !> used.  Port of follow.el:follow-calc-win-end (on a text terminal the
    !> last line of a window is never partly visible, so the position of
    !> its start is always visible).
    subroutine calculate_follow_window_end(end_position, end_of_buffer, window)
        integer(int64), intent(out) :: end_position
        logical, intent(out) :: end_of_buffer
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w
        type(status_type) :: st

        w = selected_window()
        if (present(window)) w = window
        end_position = window_end(w, st, update=.true.)
        if (.not. st%ok()) end_position = start_of(w)
        end_of_buffer = is_position_visible_in_window(point_maximum(), w)
    end subroutine calculate_follow_window_end

    !> Determine the start of window WINDOW in a Follow mode window chain.
    !> WINDOWS is a list of chained windows, and POSITION is the starting
    !> position for the first window in the list.  If WINDOW is the null
    !> handle, return the point below all windows.  Port of
    !> follow.el:follow-calc-win-start.
    integer(int64) function calculate_follow_window_start(list, position, window) result(start)
        type(handle_type), intent(in) :: list(:)
        integer(int64), intent(in) :: position
        type(handle_type), intent(in) :: window
        integer(int64) :: old_start
        logical :: end_of_buffer
        integer :: k

        start = position

        do k = 1, size(list)
            if (same_window(list(k), window)) exit
            old_start = start_of(list(k))
            call set_start_of(list(k), start, noforce=.true.)
            call calculate_follow_window_end(start, end_of_buffer, list(k))
            call set_start_of(list(k), old_start, noforce=.true.)
        end do
    end function calculate_follow_window_start

    !> Test if the cached value of `follow_windows_start_end' can be used.
    !> Note that this handles the case when the cache has been set to
    !> unallocated.  Port of follow.el:follow-cache-valid-p.
    logical function is_follow_cache_valid(list) result(valid)
        type(handle_type), intent(in) :: list(:)
        integer :: k

        valid = .false.

        if (.not. allocated(follow_windows_start_end_cache)) then
            valid = size(list) == 0
            return
        end if

        if (size(follow_windows_start_end_cache) /= size(list)) return

        do k = 1, size(list)
            if (.not. same_window(list(k), follow_windows_start_end_cache(k)%window)) return
            if (start_of(list(k)) /= follow_windows_start_end_cache(k)%start) return
        end do

        valid = .true.
    end function is_follow_cache_valid

    !> Return a list of (WINDOW START END BUFFER-END-P) for window list
    !> WINDOWS.  Port of follow.el:follow-windows-start-end (the frame's
    !> selected window is selected again).
    function follow_windows_start_end(list) result(wse)
        type(handle_type), intent(in) :: list(:)
        type(window_start_end_type), allocatable :: wse(:)
        type(handle_type) :: original_window
        integer :: k

        if (is_follow_cache_valid(list)) then
            if (allocated(follow_windows_start_end_cache)) then
                wse = follow_windows_start_end_cache
            else
                allocate (wse(0))
            end if

            return
        end if

        original_window = selected_frame%selected_window
        allocate (wse(size(list)))

        do k = 1, size(list)
            call select_live_window(list(k), norecord=.true.)
            wse(k)%window = list(k)
            wse(k)%start = start_of(list(k))
            call calculate_follow_window_end(wse(k)%end, wse(k)%buffer_end)
        end do

        call select_live_window(original_window, norecord=.true.)
        follow_windows_start_end_cache = wse
    end function follow_windows_start_end

    !> The index of WINDOW's entry in WSE, or 0.  Not a port.
    integer function entry_of(window, wse) result(k)
        type(handle_type), intent(in) :: window
        type(window_start_end_type), intent(in) :: wse(:)

        do k = 1, size(wse)
            if (same_window(wse(k)%window, window)) return
        end do

        k = 0
    end function entry_of

    !> True when POSITION is visible in WINDOW.  Port of follow.el:follow-pos-visible.
    logical function is_follow_position_visible(position, window, wse) result(visible)
        integer(int64), intent(in) :: position
        type(handle_type), intent(in) :: window
        type(window_start_end_type), intent(in) :: wse(:)
        integer :: k

        visible = .false.
        k = entry_of(window, wse)
        if (k == 0) return
        visible = position >= wse(k)%start .and. (position < wse(k)%end .or. wse(k)%buffer_end)
    end function is_follow_position_visible

    !> True if the follower windows are aligned.
    !> The argument, WSE, should be an array as
    !> returned by `follow_windows_start_end'.  Port of
    !> follow.el:follow-windows-aligned-p.
    logical function is_follow_windows_aligned(wse) result(aligned)
        type(window_start_end_type), intent(in) :: wse(:)
        integer :: k

        aligned = .true.

        do k = 1, size(wse) - 1
            if (wse(k)%end /= wse(k + 1)%start) then
                aligned = .false.
                return
            end if
        end do
    end function is_follow_windows_aligned

    !> True when the `window_point' is visible in all windows.  Port of
    !> follow.el:follow-point-visible-all-windows-p.
    logical function is_follow_point_visible_all_windows(wse) result(visible)
        type(window_start_end_type), intent(in) :: wse(:)
        integer :: k

        visible = .true.

        do k = 1, size(wse)
            if (.not. is_follow_position_visible(point_of(wse(k)%window), wse(k)%window, wse)) then
                visible = .false.
                return
            end if
        end do
    end function is_follow_point_visible_all_windows

    !> Make sure that the start of WINDOW starts at a full screen line.
    !> Port of follow.el:follow-update-window-start.
    subroutine update_follow_window_start(window_handle)
        type(handle_type), intent(in) :: window_handle
        integer(int64) :: original_point, moved

        original_point = point()
        call goto_character(start_of(window_handle))

        if (.not. is_beginning_of_line()) then
            call vertical_motion(0_int64, moved, window=windows(window_handle%index))

            if (point() /= start_of(window_handle)) then
                call vertical_motion(1_int64, moved, window=windows(window_handle%index))
                call set_start_of(window_handle, point(), noforce=.true.)
            end if
        end if

        call goto_character(original_point)
    end subroutine update_follow_window_start

    !> Select and return a window, if DESTINATION is visible in it (the null
    !> handle for none).  Port of follow.el:follow-select-if-visible.
    function follow_select_if_visible(destination, wse) result(window)
        integer(int64), intent(in) :: destination
        type(window_start_end_type), intent(in) :: wse(:)
        type(handle_type) :: window
        integer :: k

        window = null_handle()

        do k = 1, size(wse)
            if (is_follow_position_visible(destination, wse(k)%window, wse)) then
                window = wse(k)%window
                call select_live_window(window)
                return
            end if
        end do
    end function follow_select_if_visible

    !> Select and return a window, if end is visible in it (the null handle
    !> for none).  Port of follow.el:follow-select-if-end-visible.
    function follow_select_if_end_visible(wse) result(window)
        type(window_start_end_type), intent(in) :: wse(:)
        type(handle_type) :: window
        type(status_type) :: st
        integer :: k

        window = null_handle()

        do k = 1, size(wse)
            if (point_maximum() == wse(k)%end .and. wse(k)%buffer_end) then
                if (point_maximum() == window_end(wse(k)%window, st) .and. st%ok()) then
                    window = wse(k)%window
                    call select_live_window(window)
                    return
                end if
            end if
        end do
    end function follow_select_if_end_visible

    !> Try to select one of WINDOWS without repositioning the topmost window.
    !> If one of the windows in WINDOWS contains DESTINATION, select it, call
    !> `redisplay_follow', move point to DESTINATION, and return that window.
    !> Otherwise, return the null handle.  Port of
    !> follow.el:follow-select-if-visible-from-first.
    function follow_select_if_visible_from_first(destination, list) result(window_handle)
        integer(int64), intent(in) :: destination
        type(handle_type), intent(in) :: list(:)
        type(handle_type) :: window_handle
        integer(int64), allocatable :: starts(:)
        logical, allocatable :: forced(:)
        integer(int64) :: original_point, moved, end_position
        logical :: end_p
        integer :: k

        window_handle = null_handle()
        if (size(list) == 0) return
        original_point = point()
        call goto_character(start_of(list(1)))
        call vertical_motion(0_int64, moved, window=windows(list(1)%index))

        if (destination >= point()) then
            ! save-window-excursion: the windows' starts come back.
            allocate (starts(size(list)), forced(size(list)))

            do k = 1, size(list)
                starts(k) = start_of(list(k))
                forced(k) = windows(list(k)%index)%force_start
            end do

            do k = 1, size(list)
                call set_start_of(list(k), point(), noforce=.true.)
                call calculate_follow_window_end(end_position, end_p, list(k))
                call goto_character(end_position)

                if (end_p .or. destination < point()) then
                    window_handle = list(k)
                    exit
                end if
            end do

            do k = 1, size(list)
                call set_start_of(list(k), starts(k), noforce=.true.)
                windows(list(k)%index)%force_start = forced(k)
            end do
        end if

        call goto_character(original_point)

        if (is_window_live(window_handle)) then
            call select_live_window(window_handle)
            call redisplay_follow(list, list(1))
            call goto_character(destination)
        end if
    end function follow_select_if_visible_from_first

    ! ------------------------------------------------------------------
    ! Redisplay
    ! ------------------------------------------------------------------

    !> Reposition the WINDOWS around WINDOW.
    !> Should point be too close to the roof we redisplay everything
    !> from the top.  WINDOWS should contain a list of windows to
    !> redisplay; it is assumed that WINDOW is a member of the list.
    !> Should WINDOWS be absent, the windows displaying the
    !> same buffer as WINDOW, in the current frame, are used.
    !> Should WINDOW be absent, the selected window is used.
    !> If PRESERVE_WINDOW is true, keep WINDOW itself unchanged while
    !> repositioning the other windows.  Port of follow.el:follow-redisplay.
    recursive subroutine redisplay_follow(list, window, preserve_window)
        type(handle_type), intent(in), optional :: list(:)
        type(handle_type), intent(in), optional :: window
        logical, intent(in), optional :: preserve_window
        type(handle_type), allocatable :: chain(:)
        type(handle_type) :: w
        integer(int64) :: old_window_start, try_first_start, try_window_start, start
        logical :: preserve, end_of_buffer
        integer :: k

        w = selected_window()
        if (present(window)) w = window

        if (present(list)) then
            chain = list
        else
            chain = follow_all_followers(w)
        end if

        preserve = .false.
        if (present(preserve_window)) preserve = preserve_window
        old_window_start = start_of(w)
        try_first_start = estimate_follow_first_window_start(chain, w, old_window_start)
        try_window_start = calculate_follow_window_start(chain, try_first_start, w)

        if (try_window_start == old_window_start) then
            call follow_debug_message("exact")
            start = try_first_start
        else if (try_window_start < old_window_start) then
            call follow_debug_message("above")
            start = calculate_follow_first_window_start_from_above(chain, try_first_start, w, old_window_start)
        else
            call follow_debug_message("below")
            start = calculate_follow_first_window_start_from_below(chain, try_first_start, w, old_window_start)
        end if

        do k = 1, size(chain)
            if (.not. (preserve .and. same_window(chain(k), w))) call set_start_of(chain(k), start)
            call calculate_follow_window_end(start, end_of_buffer, chain(k))
        end do
    end subroutine redisplay_follow

    !> Estimate the position of the first window.
    !> The estimate is computed by assuming that the window WINDOW, which
    !> should be a member of WINDOWS, starts at position START.  Port of
    !> follow.el:follow-estimate-first-window-start.
    integer(int64) function estimate_follow_first_window_start(list, window_handle, start) result(position)
        type(handle_type), intent(in) :: list(:)
        type(handle_type), intent(in) :: window_handle
        integer(int64), intent(in) :: start
        type(handle_type), allocatable :: test(:), succ(:)
        integer(int64) :: original_point, moved
        integer :: k

        call split_follow_followers(list, test, succ, window_handle)
        original_point = point()
        call goto_character(start)
        call vertical_motion(0_int64, moved, window=windows(window_handle%index))

        do k = 1, size(test)
            call vertical_motion(-int(window_body_lines(windows(test(k)%index)), int64), moved, &
                                 window=windows(test(k)%index))
        end do

        position = point()
        call goto_character(original_point)
    end function estimate_follow_first_window_start

    !> The start of the first window of WINDOWS that puts WINDOW's start at
    !> START or below, trying from GUESS down.  Port of
    !> follow.el:follow-calculate-first-window-start-from-above.
    integer(int64) function calculate_follow_first_window_start_from_above(list, guess, window_handle, start) result(res)
        type(handle_type), intent(in) :: list(:)
        integer(int64), intent(in) :: guess
        type(handle_type), intent(in) :: window_handle
        integer(int64), intent(in) :: start
        integer(int64) :: original_point, moved, window_start

        original_point = point()
        call goto_character(guess)

        do
            call vertical_motion(1_int64, moved, window=windows(list(1)%index))

            if (moved /= 1) then
                res = point_maximum()
                exit
            end if

            window_start = calculate_follow_window_start(list, point(), window_handle)

            if (window_start >= start) then
                res = point()
                exit
            end if
        end do

        call goto_character(original_point)
    end function calculate_follow_first_window_start_from_above

    !> The start of the first window of WINDOWS that puts WINDOW's start at
    !> START, or the last one above it, trying from GUESS up.  Port of
    !> follow.el:follow-calculate-first-window-start-from-below.
    integer(int64) function calculate_follow_first_window_start_from_below(list, guess, window_handle, start) result(res)
        type(handle_type), intent(in) :: list(:)
        integer(int64), intent(in) :: guess
        type(handle_type), intent(in) :: window_handle
        integer(int64), intent(in) :: start
        integer(int64) :: saved, original_point, moved, window_start

        saved = point()
        call goto_character(guess)
        call vertical_motion(0_int64, moved, window=windows(list(1)%index))
        res = point()

        do
            original_point = point()
            call vertical_motion(-1_int64, moved, window=windows(list(1)%index))

            if (moved /= -1) then
                res = point_minimum()
                exit
            end if

            window_start = calculate_follow_window_start(list, point(), window_handle)

            if (point() >= original_point) then
                res = point()
                exit
            else if (window_start == start) then
                ! Perfect match, use this value.
                res = point()
                exit
            else if (window_start < start) then
                ! Walked to far, use previous result.
                exit
            end if

            ! Store result for next iteration.
            res = point()
        end do

        call goto_character(saved)
    end function calculate_follow_first_window_start_from_below

    !> Make sure windows displaying the end of a buffer aren't recentered.
    !> This is done by reading and rewriting the start position of
    !> non-first windows in Follow mode.  Port of
    !> follow.el:follow-avoid-tail-recenter.
    recursive subroutine avoid_follow_tail_recenter()
        type(handle_type), allocatable :: leaves(:), seen(:)
        type(handle_type) :: original_buffer, top, window
        type(status_type) :: st
        integer(int64) :: start
        integer :: n

        original_buffer = current_buffer()
        call window_leaves(selected_frame%root_window, leaves)
        if (size(leaves) == 0) return
        top = leaves(1)
        if (windows(top%index)%mini) return
        allocate (seen(size(leaves)))
        n = 0
        window = top

        do
            start = start_of(window)
            call set_buffer(windows(window%index)%buffer, st)

            ! (Each buffer once: one frame.)
            if (.not. any(seen(1:n)%index == windows(window%index)%buffer%index .and. &
                          seen(1:n)%generation == windows(window%index)%buffer%generation)) then
                n = n + 1
                seen(n) = windows(window%index)%buffer
            else if (minor_modes%follow_mode%enabled() .and. point_maximum() == start) then
                call set_start_of(window, start)
            end if

            window = next_window(window, MINIBUF_NO, st)
            if (.not. st%ok() .or. same_window(window, top) .or. n >= size(seen)) exit
        end do

        if (is_buffer_live(original_buffer)) call set_buffer(original_buffer, st)
    end subroutine avoid_follow_tail_recenter

    !> `avoid_follow_tail_recenter' as a function of
    !> `window_scroll_functions' (which ignores its arguments).  Not a port.
    recursive subroutine avoid_follow_tail_recenter_on_scroll(window, position, status)
        type(handle_type), intent(in) :: window
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status

        status = status_type()
        call avoid_follow_tail_recenter()
    end subroutine avoid_follow_tail_recenter_on_scroll

    !> Follow mode's `pre_redisplay_function': `follow_post_command_hook'
    !> when the selected window is redisplayed.  Port of
    !> follow.el:follow-pre-redisplay-function.
    recursive subroutine adjust_follow_windows(all, list)
        logical, intent(in) :: all
        type(handle_type), intent(in) :: list(:)
        type(handle_type) :: sw
        integer :: k
        logical :: found

        sw = selected_window()
        found = all .or. size(list) == 0

        do k = 1, size(list)
            if (same_window(list(k), sw)) found = .true.
        end do

        if (found) call follow_post_command_hook()
    end subroutine adjust_follow_windows

    !> Ensure that the windows in Follow mode are adjacent after each
    !> command.  Port of follow.el:follow-post-command-hook.
    recursive subroutine follow_post_command_hook()
        type(handle_type) :: window, old
        type(status_type) :: st
        logical :: saved

        if (is_input_pending()) return
        saved = follow_inside_post_command_hook_call
        follow_inside_post_command_hook_call = .true.
        window = selected_window()
        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, st)

        if (st%ok()) then
            if (.not. associated(this_command)) then
                if (allocated(follow_windows_start_end_cache)) deallocate (follow_windows_start_end_cache)
            else if (.not. this_command%follow_mode_use_cache) then
                if (allocated(follow_windows_start_end_cache)) deallocate (follow_windows_start_end_cache)
            end if

            call adjust_follow_window(window)
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
        follow_inside_post_command_hook_call = saved
    end subroutine follow_post_command_hook

    !> Adjust the window WINDOW and its followers.  Port of
    !> follow.el:follow-adjust-window.
    recursive subroutine adjust_follow_window(window)
        type(handle_type), intent(in) :: window
        type(handle_type), allocatable :: chain(:)
        type(window_start_end_type), allocatable :: wse(:)
        type(handle_type) :: found
        integer(int64) :: destination, p, original_point
        logical :: aligned, visible, selected_window_up_to_date, done

        if (minor_modes%follow_mode%enabled() .and. .not. windows(window%index)%mini) then
            chain = follow_all_followers(window)

            ! If a scrolling command has been executed, the window should
            ! not be moved.
            if (follow_fixed_window) then
                call follow_debug_message("fixed")
                call redisplay_follow(chain, window)
                call goto_character(get_follow_scrolled_point(point(), chain))
                follow_fixed_window = .false.
            end if

            destination = point()
            call update_follow_window_start(chain(1))
            wse = follow_windows_start_end(chain)
            aligned = is_follow_windows_aligned(wse)
            visible = is_follow_position_visible(destination, window, wse)
            selected_window_up_to_date = .false.

            if (.not. (aligned .and. visible)) then
                if (allocated(follow_windows_start_end_cache)) deallocate (follow_windows_start_end_cache)
            end if

            ! Select a window to display point.
            if (.not. follow_internal_force_redisplay) then
                if (destination == point_maximum()) then
                    ! At point-max, we have to be careful since the display
                    ! can be maximized by the end being visible.
                    if (visible .and. aligned .and. .not. is_deleting_command()) then
                        ! Just show the end of the window.
                        call follow_debug_message("Max: same")
                    else
                        found = follow_select_if_end_visible(wse)

                        if (is_window_live(found)) then
                            call follow_debug_message("Max: end visible")
                            visible = .true.
                            aligned = .false.
                            call goto_character(destination)
                        else
                            ! Otherwise select the last window, and
                            ! redisplay.
                            call follow_debug_message("Max: default")
                            call select_live_window(chain(size(chain)))
                            call goto_character(destination)
                            visible = .false.
                            aligned = .false.
                        end if
                    end if
                else if (visible .and. aligned) then
                    ! We're not at the end, here life is much simpler.
                    call follow_debug_message("same")
                else
                    found = follow_select_if_visible(destination, wse)

                    if (is_window_live(found)) then
                        ! If point is visible in one of the windows, select
                        ! it.
                        call follow_debug_message("visible")
                        call goto_character(destination)
                        visible = .false.
                    else if (visible) then
                        ! Not aligned, but point is visible in the selected
                        ! window.
                        call follow_debug_message("visible in selected.")
                    else if (destination < wse(1)%start) then
                        ! Point is above the first window: select it, and
                        ! redisplay.
                        call follow_debug_message("before first")
                        call select_live_window(chain(1))
                        call goto_character(destination)
                        visible = .false.
                        aligned = .false.
                    else
                        ! If we can position the cursor without moving the
                        ! first window, do it.
                        found = follow_select_if_visible_from_first(destination, chain)

                        if (is_window_live(found)) then
                            call follow_debug_message("Below first")
                            visible = .true.
                            aligned = .true.
                        else
                            ! Otherwise, redisplay with point in the
                            ! selected window.
                            call follow_debug_message("None")
                            visible = .false.
                            aligned = .false.
                        end if
                    end if
                end if

                ! If a new window was selected, make sure that the old is
                ! not scrolled when point is outside the window.
                if (.not. same_window(window, selected_window())) then
                    p = point_of(window)
                    call set_start_of(window, start_of(window))
                    ! (No line of a text terminal is partly visible:
                    ! pos-visible-in-window-p's RTOP is never there.)
                    call set_point_of(window, p)
                end if
            end if

            if (.not. visible) then
                ! If point may not be visible in the selected window,
                ! perform a redisplay; this ensures scrolling.
                original_point = point()
                done = redisplay()

                ! If this `redisplay' moved point, we got clobbered by a
                ! previous call to `set-window-start'.  Try again.
                if (point() /= original_point) then
                    call goto_character(original_point)
                    done = redisplay()
                end if

                selected_window_up_to_date = .true.
                call avoid_follow_tail_recenter()
                wse = follow_windows_start_end(chain)
                if (allocated(follow_windows_start_end_cache)) deallocate (follow_windows_start_end_cache)
                aligned = .false.
            end if

            ! Now redraw the windows around the selected window.
            if (.not. (.not. follow_internal_force_redisplay .and. (aligned .or. is_follow_windows_aligned(wse)) &
                       .and. is_follow_point_visible_all_windows(wse))) then
                follow_internal_force_redisplay = .false.
                call redisplay_follow(chain, selected_window(), selected_window_up_to_date)
                wse = follow_windows_start_end(chain)
                if (allocated(follow_windows_start_end_cache)) deallocate (follow_windows_start_end_cache)

                ! When the point ends up in another window.  This happens
                ! when dest is in the beginning of the file and the
                ! selected window is not the first.  It can also, in rare
                ! situations happen when long lines are used and there is
                ! a big difference between the width of the windows.
                if (.not. is_follow_position_visible(destination, window, wse)) then
                    found = follow_select_if_visible(destination, wse)
                    call goto_character(destination)
                end if
            end if

            ! If the region is visible, make it look good when spanning
            ! multiple windows.
            if (is_region_active()) call maximize_follow_region(selected_window(), chain, wse)
        end if

        ! Whether or not the buffer was in follow mode, update windows
        ! displaying the tail so that Emacs won't recenter them.
        call avoid_follow_tail_recenter()
    end subroutine adjust_follow_window

    !> Whether this command is one that deletes backward (they may leave
    !> point at the end of a window).  Port of the memq in
    !> follow.el:follow-adjust-window.
    logical function is_deleting_command() result(deleting)
        deleting = .false.
        if (.not. associated(this_command)) return
        deleting = associated(this_command, commands%delete_backward_character) .or. &
                   associated(this_command, commands%backward_delete_character_untabify) .or. &
                   associated(this_command, commands%cut_region)
    end function is_deleting_command

    !> Make a highlighted region stretching multiple windows look good.
    !> Port of follow.el:follow-maximize-region.
    subroutine maximize_follow_region(window, list, wse)
        type(handle_type), intent(in) :: window
        type(handle_type), intent(in) :: list(:)
        type(window_start_end_type), intent(in) :: wse(:)
        type(handle_type), allocatable :: test(:), succ(:)
        integer :: k, e

        call split_follow_followers(list, test, succ, window)

        do k = 1, size(test)
            e = entry_of(test(k), wse)
            if (e > 0) call set_point_of(test(k), max(wse(e)%start, wse(e)%end - 1))
        end do

        do k = 1, size(succ)
            e = entry_of(succ(k), wse)
            if (e > 0) call set_point_of(succ(k), wse(e)%start)
        end do
    end subroutine maximize_follow_region

    ! ------------------------------------------------------------------
    ! Scrolling
    ! ------------------------------------------------------------------

    !> Calculate the correct value for point after a scrolling operation.
    !> DESTINATION is our default position, typically where point was before the
    !> scroll.  WINDOWS is the list of windows in the follow chain.
    !>
    !> This function attempts to duplicate the point placing from
    !> `window_scroll_line_based' in the Emacs core source window.c.
    !>
    !> Return the new position.  Port of follow.el:follow-get-scrolled-point
    !> (`scroll_preserve_screen_position' .false.).
    integer(int64) function get_follow_scrolled_point(destination, list) result(position)
        integer(int64), intent(in) :: destination
        type(handle_type), intent(in) :: list(:)
        type(handle_type) :: original
        type(status_type) :: st
        integer(int64) :: original_point, destination_column, limit0, limitn, moved, column

        position = destination
        original_point = point()
        call goto_character(destination)
        column = current_column()
        call vertical_motion(0_int64, moved)
        destination_column = column - current_column()
        call goto_character(original_point)
        original = selected_frame%selected_window
        ! with-selected-window: the first window's start ...
        call select_live_window(list(1), norecord=.true.)
        original_point = point()
        call goto_character(start_of(list(1)))
        call vertical_motion(0_int64, moved)
        limit0 = point()
        call goto_character(original_point)
        ! ... and the last window's end.
        call select_live_window(list(size(list)), norecord=.true.)
        original_point = point()
        limitn = window_end(list(size(list)), st, update=.true.)
        if (.not. st%ok()) limitn = point_maximum()
        call goto_character(limitn)
        ! (Short of point-max.)
        if (.not. is_position_visible_in_window(point(), list(size(list)))) limitn = point() - 1
        call goto_character(original_point)

        if (destination < limit0) then
            call select_live_window(list(1), norecord=.true.)
            original_point = point()
            call goto_character(limit0)
            call vertical_motion(0_int64, moved, int(destination_column))
            position = point()
            call goto_character(original_point)
        else if (destination > limitn) then
            original_point = point()
            call goto_character(limitn)
            call vertical_motion(0_int64, moved, int(destination_column))
            position = point()
            call goto_character(original_point)
        end if

        call select_live_window(original, norecord=.true.)
    end function get_follow_scrolled_point

    !> Scroll the text in a follow mode window chain up by ARGUMENT lines.
    !> If ARGUMENT is no prefix, scroll the size of the current window.
    !>
    !> This is an internal function for `scroll_follow_up' and
    !> `scroll_follow_up_window'.  Port of follow.el:follow-scroll-up-arg.
    recursive subroutine scroll_follow_up_argument(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type) :: owin
        type(status_type) :: st
        integer(int64) :: original_point

        original_point = point()
        owin = selected_window()

        do
            call scroll_up_command(argument, status)
            if (status%code /= END_OF_BUFFER_CODE) exit
            ! If we are too near END_OF_BUFFER, try scrolling the previous window.
            call previous_follow_window(st)

            if (.not. st%ok()) then
                call select_live_window(owin)
                call goto_character(original_point)
                call set_error(status, END_OF_BUFFER_CODE)
                return
            end if
        end do

        if (.not. status%ok()) return
        call goto_character(original_point)
        follow_fixed_window = .true.
    end subroutine scroll_follow_up_argument

    !> Scroll the text in a follow mode window chain down by ARGUMENT lines.
    !> If ARGUMENT is no prefix, scroll the size of the current window.
    !>
    !> This is an internal function for `scroll_follow_down' and
    !> `scroll_follow_down_window'.  Port of follow.el:follow-scroll-down-arg.
    recursive subroutine scroll_follow_down_argument(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64) :: original_point

        original_point = point()
        call scroll_down_command(argument, status)
        if (.not. status%ok()) return
        call goto_character(original_point)
        follow_fixed_window = .true.
    end subroutine scroll_follow_down_argument

    !> Scroll text in a Follow mode window up by that window's size.
    !> The other windows in the window chain will scroll synchronously.
    !>
    !> If called with no ARGUMENT, the `next_screen_context_lines' last lines of
    !> the window will be visible after the scroll.
    !>
    !> If called with an argument, scroll ARGUMENT lines up.
    !> Negative ARGUMENT means scroll downward.
    !>
    !> Works like `scroll_up' when not in Follow mode.  Port of
    !> follow.el:follow-scroll-up-window.
    recursive subroutine scroll_follow_up_window(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (.not. minor_modes%follow_mode%enabled()) then
            call scroll_up_command(argument, status)
        else if (argument%kind == PREFIX_MINUS) then
            call scroll_follow_down_window(prefix_argument_type(), status)
        else
            call scroll_follow_up_argument(argument, status)
        end if
    end subroutine scroll_follow_up_window

    !> Scroll text in a Follow mode window down by that window's size.
    !> The other windows in the window chain will scroll synchronously.
    !>
    !> If called with no ARGUMENT, the `next_screen_context_lines' top lines of
    !> the window in the chain will be visible after the scroll.
    !>
    !> If called with an argument, scroll ARGUMENT lines down.
    !> Negative ARGUMENT means scroll upward.
    !>
    !> Works like `scroll_down' when not in Follow mode.  Port of
    !> follow.el:follow-scroll-down-window.
    recursive subroutine scroll_follow_down_window(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (.not. minor_modes%follow_mode%enabled()) then
            call scroll_down_command(argument, status)
        else if (argument%kind == PREFIX_MINUS) then
            call scroll_follow_up_window(prefix_argument_type(), status)
        else
            call scroll_follow_down_argument(argument, status)
        end if
    end subroutine scroll_follow_down_window

    !> Scroll text in a Follow mode window chain up.
    !>
    !> If called with no ARGUMENT, the `next_screen_context_lines' last lines of
    !> the bottom window in the chain will be visible in the top window.
    !>
    !> If called with an argument, scroll ARGUMENT lines up.
    !> Negative ARGUMENT means scroll downward.
    !>
    !> Works like `scroll_up' when not in Follow mode.  Port of
    !> follow.el:follow-scroll-up.
    recursive subroutine scroll_follow_up(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)
        integer(int64) :: end, moved

        status = status_type()

        if (.not. minor_modes%follow_mode%enabled()) then
            call scroll_up_command(argument, status)
        else if (argument%kind /= PREFIX_NONE) then
            call scroll_follow_up_argument(argument, status)
        else
            chain = follow_all_followers()
            end = window_end(chain(size(chain)), status)
            if (.not. status%ok()) return

            if (end == point_maximum()) then
                if (.not. scroll_error_top_bottom .or. is_end_of_buffer()) then
                    call set_error(status, END_OF_BUFFER_CODE)
                else
                    call goto_character(point_maximum())
                end if
            else
                call select_live_window(chain(1))
                call goto_character(end)
                call vertical_motion(-int(next_screen_context_lines, int64), moved)
                call set_start_of(chain(1), point())
            end if
        end if
    end subroutine scroll_follow_up

    !> Scroll text in a Follow mode window chain down.
    !>
    !> If called with no ARGUMENT, the `next_screen_context_lines' top lines of
    !> the top window in the chain will be visible in the bottom window.
    !>
    !> If called with an argument, scroll ARGUMENT lines down.
    !> Negative ARGUMENT means scroll upward.
    !>
    !> Works like `scroll_down' when not in Follow mode.  Port of
    !> follow.el:follow-scroll-down (no tab line).
    recursive subroutine scroll_follow_down(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)
        integer(int64) :: original_point, start, lines, moved
        integer :: k
        logical :: at_top

        status = status_type()

        if (.not. minor_modes%follow_mode%enabled()) then
            call scroll_down_command(argument, status)
        else if (argument%kind /= PREFIX_NONE) then
            call scroll_follow_down_argument(argument, status)
        else
            original_point = point()
            chain = follow_all_followers()
            start = start_of(chain(1))
            lines = 0

            if (start == point_minimum()) then
                if (.not. scroll_error_top_bottom .or. is_beginning_of_buffer()) then
                    call set_error(status, BEGINNING_OF_BUFFER_CODE)
                else
                    call goto_character(point_minimum())
                end if
            else
                call select_live_window(chain(1))

                do k = 1, size(chain)
                    ! Count mode-line, too.
                    lines = lines + windows(chain(k)%index)%total_lines - 1
                    if (buffers(current)%header_line_format%is_set()) lines = lines - 1
                    if (buffers(current)%tab_line_format%is_set()) lines = lines - 1
                end do

                lines = lines - next_screen_context_lines
                call goto_character(start)
                call vertical_motion(-lines, moved)
                at_top = moved > -lines
                call set_start_of(chain(1), point())

                if (at_top) then
                    call goto_character(original_point)
                else
                    call goto_character(start)
                    call vertical_motion(int(next_screen_context_lines - 1, int64), moved)
                    if (original_point < point()) call goto_character(original_point)
                end if

                follow_internal_force_redisplay = .true.
            end if
        end if
    end subroutine scroll_follow_down

    ! ------------------------------------------------------------------
    ! User functions
    ! ------------------------------------------------------------------

    !> Create two side by side windows and enter Follow mode.
    !>
    !> Execute this command to display as much as possible of the text
    !> in the selected window.  All other windows, in the current
    !> frame, are deleted and the selected window is split in two
    !> side-by-side windows.  Follow mode is activated, hence the
    !> two windows always will display two successive pages.
    !> (If one window is moved, the other one will follow.)
    !>
    !> If ARGUMENT is positive, the leftmost window is selected.  If negative,
    !> the rightmost is selected.  If ARGUMENT is no prefix, the leftmost window
    !> is selected if the original window is the first one in the frame.
    !> Port of follow.el:follow-delete-other-windows-and-split.
    recursive subroutine follow_delete_other_windows_and_split(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: new
        integer(int64) :: start
        logical :: other

        call window_leaves(selected_frame%root_window, leaves)

        if (argument%kind == PREFIX_NONE) then
            ! (frame-first-window)
            other = .not. same_window(selected_window(), leaves(1))
        else
            other = prefix_numeric_value(argument) < 0
        end if

        start = start_of(selected_window())
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        ! (split-window-right)
        call split_window(selected_window(), 0, .false., .true., new, status)
        if (.not. status%ok()) return

        if (other) then
            call other_window(1_int64, status)
            if (.not. status%ok()) return
            call set_start_of(selected_window(), start)
            follow_internal_force_redisplay = .true.
        end if

        call minor_modes%follow_mode%enable(status)
    end subroutine follow_delete_other_windows_and_split

    !> Show BUFFER in all windows in the current Follow mode window chain.
    !> Port of follow.el:follow-switch-to-buffer.
    recursive subroutine follow_switch_to_buffer(buffer, status)
        character(len=*), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)
        type(handle_type) :: original_window
        integer :: k

        status = status_type()
        original_window = selected_window()
        chain = follow_all_followers()

        do k = 1, size(chain)
            call select_live_window(chain(k))
            call switch_to_buffer(buffer, status)
            if (.not. status%ok()) return
        end do

        call select_live_window(original_window)
    end subroutine follow_switch_to_buffer

    !> Show BUFFER in all windows on this frame.
    !> Defaults to current buffer.  Port of
    !> follow.el:follow-switch-to-buffer-all.
    recursive subroutine follow_switch_to_buffer_all(status, buffer)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: original_window
        character(len=:), allocatable :: name
        integer :: k

        status = status_type()

        if (present(buffer)) then
            name = buffer
        else
            name = buffers(current)%name
        end if

        original_window = selected_window()
        ! walk-windows (no minibuffer window).
        list = window_list_1(selected_window(), MINIBUF_NO, status)
        if (.not. status%ok()) return

        do k = 1, size(list)
            call select_live_window(list(k))
            call switch_to_buffer(name, status)
            if (.not. status%ok()) exit
        end do

        call select_live_window(original_window, norecord=.true.)
        if (.not. status%ok()) return
        call select_live_window(original_window)
        call redisplay_follow()
    end subroutine follow_switch_to_buffer_all

    !> Show current buffer in all windows on this frame, and enter Follow
    !> mode.  Port of follow.el:follow-switch-to-current-buffer-all.
    recursive subroutine follow_switch_to_current_buffer_all(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. minor_modes%follow_mode%enabled()) then
            call minor_modes%follow_mode%enable(status)
            if (.not. status%ok()) return
        end if

        call follow_switch_to_buffer_all(status)
    end subroutine follow_switch_to_current_buffer_all

    !> Select the next window showing the same buffer.  Port of
    !> follow.el:follow-next-window.
    recursive subroutine next_follow_window(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: test(:), succ(:)

        status = status_type()
        call split_follow_followers(follow_all_followers(), test, succ)

        if (size(succ) > 0) then
            call select_live_window(succ(1))
        else
            call set_error(status, GENERIC_ERROR_CODE, "No more windows")
        end if
    end subroutine next_follow_window

    !> Select the previous window showing the same buffer.  Port of
    !> follow.el:follow-previous-window.
    recursive subroutine previous_follow_window(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: test(:), succ(:)

        status = status_type()
        call split_follow_followers(follow_all_followers(), test, succ)

        if (size(test) > 0) then
            call select_live_window(test(1))
        else
            call set_error(status, GENERIC_ERROR_CODE, "No more windows")
        end if
    end subroutine previous_follow_window

    !> Select the first window in the frame showing the same buffer.
    !> Port of follow.el:follow-first-window.
    subroutine follow_first_window(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)

        status = status_type()
        chain = follow_all_followers()
        call select_live_window(chain(1))
    end subroutine follow_first_window

    !> Select the last window in the frame showing the same buffer.  Port of
    !> follow.el:follow-last-window.
    subroutine follow_last_window(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)

        status = status_type()
        chain = follow_all_followers()
        call select_live_window(chain(size(chain)))
    end subroutine follow_last_window

    !> Recenter the middle window around point.
    !> Rearrange all other windows around the middle window.
    !>
    !> With a positive argument, place the current line ARGUMENT lines
    !> from the top.  With a negative argument, place it -ARGUMENT lines
    !> from the bottom.  Port of follow.el:follow-recenter.
    subroutine recenter_follow(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chain(:)
        integer(int64) :: p, n, destination, window_s

        if (argument%kind /= PREFIX_NONE) then
            p = point()
            n = prefix_numeric_value(argument)

            if (n >= 0) then
                call follow_first_window(status)
                call goto_character(p)
                call recenter(prefix_argument_type(PREFIX_NUMBER, n), .false., status)
            else
                call follow_last_window(status)
                call goto_character(p)
                call recenter(prefix_argument_type(PREFIX_NUMBER, n), .false., status)
                ! If we happen to have just two windows, we'll have to
                ! move the other windows' start, so that the last window
                ! can be recentered.
                follow_internal_force_redisplay = .true.
            end if
        else
            destination = point()
            chain = follow_all_followers()
            call select_live_window(chain((size(chain) - 1)/2 + 1))
            window_s = start_of(selected_window())
            call goto_character(destination)
            call recenter(prefix_argument_type(), .false., status)
            ! If the middle window's start would need to move up the
            ! windows above it: they are redisplayed.
            if (destination < window_s) follow_internal_force_redisplay = .true.
        end if
    end subroutine recenter_follow

    !> Arrange windows displaying the same buffer in successor order.
    !> This function can be called even if the buffer is not in Follow mode.
    !>
    !> Hopefully, there should be no reason to call this function when in
    !> Follow mode since the windows should always be aligned.  Port of
    !> follow.el:follow-redraw.
    recursive subroutine redraw_follow(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call sit_for(0)
        call redisplay_follow()
    end subroutine redraw_follow

    !> Move point to the end of the buffer, Follow mode style.
    !>
    !> If the end is not visible, it will be displayed in the last possible
    !> window in the Follow mode window chain.
    !>
    !> The mark is left at the previous position.  With arg N, put point N/10
    !> of the way from the true end.  Port of follow.el:follow-end-of-buffer.
    subroutine follow_end_of_buffer(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: followers(:)
        type(handle_type) :: found
        integer(int64) :: position

        status = status_type()
        followers = follow_all_followers()
        position = point()

        if (argument%kind /= PREFIX_NONE) then
            call select_live_window(followers(size(followers)))
        else
            found = follow_select_if_end_visible(follow_windows_start_end(followers))
            if (.not. is_window_live(found)) call select_live_window(followers(size(followers)))
        end if

        call goto_character(position)
        call end_of_buffer(argument)
    end subroutine follow_end_of_buffer

    ! ------------------------------------------------------------------
    ! The commands
    ! ------------------------------------------------------------------

    subroutine execute_scroll_follow_up_window(status)
        type(status_type), intent(out) :: status

        call scroll_follow_up_window(current_prefix_argument, status)
    end subroutine execute_scroll_follow_up_window

    subroutine execute_scroll_follow_down_window(status)
        type(status_type), intent(out) :: status

        call scroll_follow_down_window(current_prefix_argument, status)
    end subroutine execute_scroll_follow_down_window

    subroutine execute_scroll_follow_up(status)
        type(status_type), intent(out) :: status

        call scroll_follow_up(current_prefix_argument, status)
    end subroutine execute_scroll_follow_up

    subroutine execute_scroll_follow_down(status)
        type(status_type), intent(out) :: status

        call scroll_follow_down(current_prefix_argument, status)
    end subroutine execute_scroll_follow_down

    subroutine execute_follow_delete_other_windows_and_split(status)
        type(status_type), intent(out) :: status

        call follow_delete_other_windows_and_split(current_prefix_argument, status)
    end subroutine execute_follow_delete_other_windows_and_split

    !> The command: the buffer is read (a name that may be new, the other
    !> buffer by default).  Port of follow.el:follow-switch-to-buffer (the
    !> command).
    recursive subroutine execute_follow_switch_to_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: def
        integer(int64) :: bi

        def = other_buffer(current_buffer())
        call resolve_buffer(def, bi, status)
        if (.not. status%ok()) return
        call read_buffer("Switch to Buffer: ", name, status, def=buffers(bi)%name)
        if (.not. status%ok()) return
        call follow_switch_to_buffer(name, status)
    end subroutine execute_follow_switch_to_buffer

    !> The command: the buffer is read, the current buffer by default.
    !> Port of follow.el:follow-switch-to-buffer-all (the command).
    recursive subroutine execute_follow_switch_to_buffer_all(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_buffer("Switch to Buffer: ", name, status, def=buffers(current)%name)
        if (.not. status%ok()) return
        call follow_switch_to_buffer_all(status, name)
    end subroutine execute_follow_switch_to_buffer_all

    subroutine execute_recenter_follow(status)
        type(status_type), intent(out) :: status

        call recenter_follow(current_prefix_argument, status)
    end subroutine execute_recenter_follow

    subroutine execute_follow_end_of_buffer(status)
        type(status_type), intent(out) :: status

        call follow_end_of_buffer(current_prefix_argument, status)
    end subroutine execute_follow_end_of_buffer

    !> Define Follow mode, its keymaps and commands, and put
    !> `avoid_follow_tail_recenter' on `window_scroll_functions'.  Not a port.
    subroutine initialize_follow()
        type(status_type) :: st

        call minor_modes%follow_mode%define("follow_mode", trim(follow_mode_line_text), hook=follow_mode_hook, &
                                            keymap=.true., body=follow_mode_body)
        follow_mode_map = minor_modes%follow_mode%map
        call register_keymap_name("follow_mode_map", follow_mode_map)
        call make_sparse_keymap(follow_mode_submap)
        call register_keymap_name("follow_mode_submap", follow_mode_submap)
        call keymap_set(follow_mode_submap, "C-v", "scroll_follow_up", st)
        call keymap_set(follow_mode_submap, "M-v", "scroll_follow_down", st)
        call keymap_set(follow_mode_submap, "v", "scroll_follow_down", st)
        call keymap_set(follow_mode_submap, "1", "follow_delete_other_windows_and_split", st)
        call keymap_set(follow_mode_submap, "b", "follow_switch_to_buffer", st)
        call keymap_set(follow_mode_submap, "C-b", "follow_switch_to_buffer_all", st)
        call keymap_set(follow_mode_submap, "C-l", "recenter_follow", st)
        call keymap_set(follow_mode_submap, "<", "follow_first_window", st)
        call keymap_set(follow_mode_submap, ">", "follow_last_window", st)
        call keymap_set(follow_mode_submap, "n", "next_follow_window", st)
        call keymap_set(follow_mode_submap, "p", "previous_follow_window", st)
        call keymap_set(follow_mode_map, trim(follow_mode_prefix_key), make_keymap_reference(follow_mode_submap), st)
        ! Replace the standard `end_of_buffer' when in Follow mode.
        call keymap_set(follow_mode_map, "<remap> <end_of_buffer>", "follow_end_of_buffer", st)
        ! The commands that don't require recalculation
        ! (follow-cache-command-list).
        commands%next_line%follow_mode_use_cache = .true.
        commands%previous_line%follow_mode_use_cache = .true.
        commands%forward_character%follow_mode_use_cache = .true.
        commands%backward_character%follow_mode_use_cache = .true.
        commands%right_character%follow_mode_use_cache = .true.
        commands%left_character%follow_mode_use_cache = .true.
        call window_scroll_functions%append(avoid_follow_tail_recenter_on_scroll)
        commands%follow_mode%execute => follow_mode
        commands%scroll_follow_up_window%execute => execute_scroll_follow_up_window
        commands%scroll_follow_down_window%execute => execute_scroll_follow_down_window
        commands%scroll_follow_up%execute => execute_scroll_follow_up
        commands%scroll_follow_down%execute => execute_scroll_follow_down
        commands%follow_delete_other_windows_and_split%execute => execute_follow_delete_other_windows_and_split
        commands%follow_switch_to_buffer%execute => execute_follow_switch_to_buffer
        commands%follow_switch_to_buffer_all%execute => execute_follow_switch_to_buffer_all
        commands%follow_switch_to_current_buffer_all%execute => follow_switch_to_current_buffer_all
        commands%next_follow_window%execute => next_follow_window
        commands%previous_follow_window%execute => previous_follow_window
        commands%follow_first_window%execute => follow_first_window
        commands%follow_last_window%execute => follow_last_window
        commands%recenter_follow%execute => execute_recenter_follow
        commands%redraw_follow%execute => redraw_follow
        commands%follow_end_of_buffer%execute => execute_follow_end_of_buffer
    end subroutine initialize_follow
end module follow_mod
