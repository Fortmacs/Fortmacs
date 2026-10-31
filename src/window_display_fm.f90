! window_display_fm.f90 -- showing buffers in windows: set-window-buffer,
! the previous buffers of windows, display-buffer and quitting windows.
! Ported from GNU Emacs 31.1 src/window.c (set_window_buffer,
! unshow_buffer, window_discard_buffer_from_window) and lisp/window.el.
!
! Copyright (C) 1985-1987, 1993-1998, 2000-2026 Free Software Foundation,
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

!> Part of window_fm: which buffer a window shows.  A window remembers
!> the buffers it showed before (`prev_buffers', with their start and
!> point) and how it came to show its buffer (`quit_restore'), so that
!> `quit_window' and `switch_to_prev_buffer' can go back.
!> `display_buffer' picks a window for a buffer the way window.el's
!> `display_buffer' does with its default actions: reuse a window
!> showing the buffer, split the largest or least recently used window
!> sensibly, use a window that showed the buffer before, or else use
!> some other window.
!>
!> Deferred: `display_buffer_table', `display_buffer_base_action' and the
!> action table entries other than `inhibit_same_window', other frames
!> and tabs (`pop_up_frames', `display_buffer_pop_up_frame'), side and
!> atomic windows, `window_size_fixed', frame buffer predicates,
!> `switch_to_prev_buffer_skip' and `switch_to_prev_buffer_skip_regexp'
!> other than nil, `quit_restore_window_no_switch',
!> `kill_buffer_quit_windows', `quit_window_hook' and
!> `quit_window_kill_buffer', `temp_buffer_resize_mode', the
!> `window_height' and `window_width' entries, nil for the split
!> thresholds, and windows resized by more than their next or previous
!> sibling can give.
submodule(window_fm) window_display_fm
    use commands_fm, only: commands
    use error_fm, only: set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR, FM_WRONG_TYPE_ARGUMENT
    use buffer_fm, only: cur, current_buffer, set_buffer, is_buffer_live, record_buffer, bury_buffer_internal, &
                         frame_buffer_list, get_buffer, get_buffer_create, kill_buffer, &
                         set_buffer_read_only, restore_buffer_modified, replace_buffer_in_windows_hook, &
                         buffer_handle
    use marker_fm, only: set_marker_restricted, copy_marker
    use frame_fm, only: selected_frame, ensure_frame_windows, selected_window
    use editfns_fm, only: goto_char
    use indent_fm, only: vertical_motion, screen_column
    use buffer_fm, only: inhibit_read_only
    use insdel_fm, only: inhibit_modification_hooks
    use data_fm, only: set
    use lread_fm, only: sym
    use lisp_fm, only: value_type, t_value
    use minibuf_fm, only: minibuf_level
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use sysdep_fm, only: emacs_clock_realtime
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Window helpers
    ! ------------------------------------------------------------------

    !> Whether window `h' is the selected window.  Not a port.
    logical function is_selected(h)
        type(handle_type), intent(in) :: h

        is_selected = same_handle(h, selected_window())
    end function is_selected

    !> WINDOW's point: its buffer's point for the selected window, else
    !> its own.  Port of window.c:Fwindow_point and window.c:window_point.
    integer(int64) function point_of(h) result(pos)
        type(handle_type), intent(in) :: h
        integer(int64) :: bi
        type(status_type) :: status

        if (is_selected(h)) then
            call resolve_buffer(windows(h%index)%buffer, bi, status)

            if (status%ok()) then
                pos = buffers(bi)%pt
                return
            end if
        end if

        pos = window_point(windows(h%index))
    end function point_of

    !> The name of the buffer of `h' (a live buffer), or "".  Not a port.
    function buffer_name_of(b) result(name)
        type(handle_type), intent(in) :: b
        character(len=:), allocatable :: name
        integer(int64) :: bi
        type(status_type) :: status

        name = ""
        call resolve_buffer(b, bi, status)
        if (status%ok()) name = buffers(bi)%name
    end function buffer_name_of

    !> The live ordinary windows of the frame in cyclic order, starting
    !> with `first' (the selected window if absent or not ordinary).
    !> Port of window.el:window-list-1, with MINIBUF `nomini'.
    function window_list_from(first) result(list)
        type(handle_type), intent(in), optional :: first
        type(handle_type), allocatable :: list(:)
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: start
        integer :: k, at, n

        call ensure_frame_windows()
        call window_leaves(selected_frame%root_window, leaves)
        n = size(leaves)
        start = selected_frame%selected_window
        if (present(first)) start = first
        at = 1

        do k = 1, n
            if (same_handle(leaves(k), start)) at = k
        end do

        allocate (list(n))

        do k = 1, n
            list(k) = leaves(modulo(at - 1 + k - 1, n) + 1)
        end do
    end function window_list_from

    !> Whether `h' is combined with siblings above and below (or with
    !> HORIZONTAL, side by side).  Port of window.el:window-combined-p.
    logical function is_window_combined(h, horizontal)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        type(handle_type) :: parent

        is_window_combined = .false.
        parent = windows(h%index)%parent
        if (.not. is_window_valid(parent)) return
        is_window_combined = windows(parent%index)%horizontal .eqv. horizontal
    end function is_window_combined

    !> The window's total height if it is combined vertically, else its
    !> width (what display-buffer-record-window stores).  Not a port.
    integer function combined_size(h)
        type(handle_type), intent(in) :: h

        if (is_window_combined(h, .false.)) then
            combined_size = windows(h%index)%total_lines
        else
            combined_size = windows(h%index)%total_cols
        end if
    end function combined_size

    !> Whether `h' is as wide as its frame.  Port of
    !> window.el:window-full-width-p.
    logical function is_window_full_width(h)
        type(handle_type), intent(in) :: h

        is_window_full_width = windows(h%index)%total_cols == windows(selected_frame%root_window%index)%total_cols
    end function is_window_full_width

    !> Columns available for text: the total minus the vertical divider.
    !> Port of window.el:window-width (window-body-width).
    integer function body_cols(h)
        type(handle_type), intent(in) :: h

        body_cols = windows(h%index)%total_cols
        if (windows(h%index)%divider) body_cols = body_cols - 1
    end function body_cols

    ! ------------------------------------------------------------------
    ! set-window-buffer and the previous buffers
    ! ------------------------------------------------------------------

    !> Make window `h' show `buffer', at the start the buffer had in the
    !> last window that showed it and with the buffer's point.  Port of
    !> window.c:set_window_buffer.
    module subroutine set_window_buffer_internal(h, buffer, keep_margins)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: keep_margins
        integer(int64) :: bi
        logical :: samebuf
        type(status_type) :: status

        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return

        associate (w => windows(h%index))
            samebuf = same_handle(buffer, w%buffer)
            w%buffer = buffer
            if (is_selected(h)) buffers(bi)%last_selected_window = h
            ! Update time stamps of buffer display.
            buffers(bi)%display_count = buffers(bi)%display_count + 1
            call emacs_clock_realtime(buffers(bi)%display_time_sec, buffers(bi)%display_time_nsec)
            buffers(bi)%display_time_set = .true.
            w%window_end_pos = 0
            w%window_end_valid = .false.
            ! Discard BUFFER from WINDOW's previous and next buffers.
            call window_discard_buffer_from_window(buffer, h, .false.)
            if (w%start%index == 0) call make_marker(w%start)
            if (w%pointm%index == 0) call make_marker(w%pointm)

            if (.not. (keep_margins .and. samebuf)) then
                w%hscroll = 0
                call set_marker(w%pointm, buffers(bi)%pt, status, buffer)
                call set_marker_restricted(w%start, buffers(bi)%last_window_start, status, buffer)
                w%force_start = .false.
            end if
        end associate
    end subroutine set_window_buffer_internal

    !> Record info on the buffer window `h' is displaying when it is about
    !> to cease to display that buffer: its window start, and its point
    !> unless the buffer is the selected window's.  Port of
    !> window.c:unshow_buffer.
    module subroutine unshow_buffer(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: bi, pos
        type(status_type) :: status
        type(handle_type) :: sel, lsw
        logical :: other_shows

        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (.not. status%ok()) return
        buffers(bi)%last_window_start = window_start(windows(h%index))
        ! Point in the selected window's buffer is actually stored in that
        ! buffer, and the window's pointm isn't used.  So don't clobber
        ! point in that buffer.
        sel = selected_window()
        lsw = buffers(bi)%last_selected_window
        other_shows = .false.

        if (is_window_live(lsw)) then
            if (.not. same_handle(lsw, h)) other_shows = same_handle(windows(lsw%index)%buffer, windows(h%index)%buffer)
        end if

        if (.not. same_handle(windows(sel%index)%buffer, windows(h%index)%buffer) .and. .not. other_shows) then
            pos = min(max(window_point(windows(h%index)), buffers(bi)%begv), buffers(bi)%zv)
            call goto_in_buffer(bi, pos)
        end if

        if (same_handle(lsw, h)) buffers(bi)%last_selected_window = null_handle()
    end subroutine unshow_buffer

    !> Set point of buffer slot `bi' to POS (temp_set_point_both).
    !> Not a port.
    subroutine goto_in_buffer(bi, pos)
        integer(int64), intent(in) :: bi, pos
        type(handle_type) :: saved
        type(status_type) :: status

        saved = current_buffer()
        call set_buffer(buffer_handle(bi), status)
        call goto_char(pos)
        if (is_buffer_live(saved)) call set_buffer(saved, status)
    end subroutine goto_in_buffer

    module subroutine set_window_buffer(h, buffer, status, keep_margins)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_margins
        logical :: keep

        status = status_type()

        if (.not. is_window_live(h)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "is_window_live")
            return
        end if

        if (.not. is_buffer_live(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to display deleted buffer")
            return
        end if

        keep = .false.
        if (present(keep_margins)) keep = keep_margins

        associate (w => windows(h%index))
            if (w%buffer%index /= 0) then
                if (.not. same_handle(w%buffer, buffer)) then
                    if (w%dedicated == DEDICATED_STRONG) then
                        ! WINDOW is strongly dedicated to its buffer.
                        call set_error(status, FM_GENERIC_ERROR, format_message("Window is dedicated to `")// &
                                       buffer_name_of(w%buffer)//format_message("'"))
                        return
                    end if

                    ! WINDOW is weakly dedicated to its buffer, reset
                    ! dedication.
                    w%dedicated = DEDICATED_NONE
                    call record_window_buffer(h)
                end if

                call unshow_buffer(h)
            end if
        end associate

        call set_window_buffer_internal(h, buffer, keep)
    end subroutine set_window_buffer

    !> Remove BUFFER from the previous and next buffers of window `h',
    !> and with ALL from its `quit_restore' and `quit_restore_prev'
    !> parameters.  Port of window.c:window_discard_buffer_from_window.
    module subroutine window_discard_buffer_from_window(buffer, h, all)
        type(handle_type), intent(in) :: buffer, h
        logical, intent(in) :: all
        type(prev_buffer_type), allocatable :: kept(:)
        type(handle_type), allocatable :: knext(:)
        integer :: k, n

        associate (w => windows(h%index))
            if (allocated(w%prev_buffers)) then
                allocate (kept(size(w%prev_buffers)))
                n = 0

                do k = 1, size(w%prev_buffers)
                    if (same_handle(w%prev_buffers(k)%buffer, buffer)) then
                        call free_prev_buffer(w%prev_buffers(k))
                    else
                        n = n + 1
                        kept(n) = w%prev_buffers(k)
                    end if
                end do

                deallocate (w%prev_buffers)
                allocate (w%prev_buffers(n))
                w%prev_buffers(1:n) = kept(1:n)
            end if

            if (allocated(w%next_buffers)) then
                allocate (knext(size(w%next_buffers)))
                n = 0

                do k = 1, size(w%next_buffers)
                    if (same_handle(w%next_buffers(k), buffer)) cycle
                    n = n + 1
                    knext(n) = w%next_buffers(k)
                end do

                deallocate (w%next_buffers)
                allocate (w%next_buffers(n))
                w%next_buffers(1:n) = knext(1:n)
            end if

            if (.not. all) return
            if (mentions(w%quit_restore_prev, buffer)) call free_quit_restore(w%quit_restore_prev)

            if (mentions(w%quit_restore, buffer)) then
                call free_quit_restore(w%quit_restore)
                ! quit-restore becomes quit-restore-prev (which keeps its
                ! marker).
                w%quit_restore = w%quit_restore_prev
                w%quit_restore_prev = quit_restore_type()
            end if
        end associate
    end subroutine window_discard_buffer_from_window

    !> Whether a `quit_restore' parameter names BUFFER as its fourth
    !> element or as the buffer of its quadruple.  Not a port.
    logical function mentions(q, buffer)
        type(quit_restore_type), intent(in) :: q
        type(handle_type), intent(in) :: buffer

        mentions = .false.
        if (q%kind == QR_NONE) return
        mentions = same_handle(q%buffer, buffer)
        if (q%has_quad) mentions = mentions .or. same_handle(q%quad_buffer, buffer)
    end function mentions

    !> Record WINDOW's buffer.
    !> Add the buffer currently shown in WINDOW to the list of WINDOW's
    !> previous buffers.  WINDOW must be a live window and defaults to the
    !> selected one.
    !>
    !> If WINDOW is not a minibuffer window, do not record insignificant
    !> buffers (buffers whose name starts with a space).  If WINDOW is a
    !> minibuffer window, record its buffer if and only if that buffer is a
    !> live minibuffer.  Port of window.el:record-window-buffer.
    module subroutine record_window_buffer(h)
        type(handle_type), intent(in) :: h
        type(prev_buffer_type), allocatable :: grown(:)
        type(prev_buffer_type) :: entry
        type(handle_type) :: buffer
        character(len=:), allocatable :: name
        integer(int64) :: start, point
        integer :: k, n, found
        type(status_type) :: status
        logical :: record

        associate (w => windows(h%index))
            buffer = w%buffer
            if (.not. allocated(w%prev_buffers)) allocate (w%prev_buffers(0))
            found = 0

            do k = 1, size(w%prev_buffers)
                if (same_handle(w%prev_buffers(k)%buffer, buffer)) found = k
            end do

            ! Reset WINDOW's next buffers.  If needed, they are resurrected
            ! by `switch_to_prev_buffer' and `switch_to_next_buffer'.
            if (allocated(w%next_buffers)) deallocate (w%next_buffers)
            allocate (w%next_buffers(0))
            name = buffer_name_of(buffer)

            ! For minibuffer windows record live minibuffers only.  For
            ! normal windows do not record insignificant buffers.
            if (w%mini) then
                record = index(name, " *Minibuf-") == 1
            else
                record = len(name) > 0
                if (record) record = name(1:1) /= " "
            end if

            if (.not. record) then
                ! The list keeps its first entry: `assq_delete_all' only
                ! splices out later ones.
                if (found > 1) then
                    call free_prev_buffer(w%prev_buffers(found))
                    call remove_entry(found)
                end if

                return
            end if

            start = window_start(w)
            point = point_of(h)

            if (found > 0) then
                ! We have an entry, update marker positions.
                entry = w%prev_buffers(found)
                call remove_entry(found)
            else
                entry%buffer = buffer
                call make_marker(entry%start)
                call make_marker(entry%point)
            end if

            call set_marker(entry%start, start, status, buffer)
            call set_marker(entry%point, point, status, buffer)
            n = size(w%prev_buffers)
            allocate (grown(n + 1))
            grown(1) = entry
            grown(2:n + 1) = w%prev_buffers
            call move_alloc(grown, w%prev_buffers)
        end associate

    contains

        !> Drop entry K from the list without freeing its markers.
        subroutine remove_entry(k)
            integer, intent(in) :: k
            type(prev_buffer_type), allocatable :: kept(:)
            integer :: m

            m = size(windows(h%index)%prev_buffers)
            allocate (kept(m - 1))
            kept(1:k - 1) = windows(h%index)%prev_buffers(1:k - 1)
            kept(k:m - 1) = windows(h%index)%prev_buffers(k + 1:m)
            call move_alloc(kept, windows(h%index)%prev_buffers)
        end subroutine remove_entry
    end subroutine record_window_buffer

    !> Unrecord BUFFER in WINDOW: make it disappear from the buffers
    !> previously shown there and, with ALL, from the `quit_restore' and
    !> `quit_restore_prev' parameters.  Port of
    !> window.el:unrecord-window-buffer.
    subroutine unrecord_window_buffer(h, buffer, all)
        type(handle_type), intent(in) :: h, buffer
        logical, intent(in) :: all

        if (is_buffer_live(buffer)) call window_discard_buffer_from_window(buffer, h, all)
    end subroutine unrecord_window_buffer

    !> Set WINDOW's buffer to BUFFER.
    !> Optional argument START non-nil means set WINDOW's start position
    !> to START.  Optional argument POINT non-nil means set WINDOW's
    !> point to POINT.  If WINDOW is selected this also sets BUFFER's
    !> `point' to POINT.  If WINDOW is selected and the buffer it showed
    !> before was current this also makes BUFFER the current buffer.
    !> Port of window.el:set-window-buffer-start-and-point.
    subroutine set_window_buffer_start_and_point(h, buffer, status, start, point)
        type(handle_type), intent(in) :: h, buffer
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, point
        logical :: selected, current
        type(handle_type) :: cb

        selected = is_selected(h)
        cb = current_buffer()
        current = same_handle(windows(h%index)%buffer, cb)
        call set_window_buffer(h, buffer, status)
        if (.not. status%ok()) return
        if (selected .and. current) call set_buffer(buffer, status)
        ! Don't force window-start here (even if POINT is nil).
        if (present(start)) call set_window_start(windows(h%index), start, status, noforce=.true.)
        if (present(point)) call set_point_of(h, point)
    end subroutine set_window_buffer_start_and_point

    !> Make POS the point of window `h': its buffer's point if it is the
    !> selected window.  Port of window.c:Fset_window_point.
    subroutine set_point_of(h, pos)
        type(handle_type), intent(in) :: h
        integer(int64), intent(in) :: pos
        type(handle_type) :: saved
        type(status_type) :: status

        if (is_selected(h)) then
            saved = current_buffer()
            call set_buffer(windows(h%index)%buffer, status)
            call goto_char(pos)
            if (is_buffer_live(saved)) call set_buffer(saved, status)
        else
            call set_window_point(windows(h%index), pos, status)
        end if
    end subroutine set_point_of

    !> The position of marker `m' (0 if it points nowhere).  Not a port.
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: status

        pos = 0
        if (m%index == 0) return
        call marker_position(m, pos, status)
        if (.not. status%ok()) pos = 0
    end function marker_pos

    ! ------------------------------------------------------------------
    ! Switching to previous and next buffers
    ! ------------------------------------------------------------------

    !> Whether `b' is in the list `list'.  Not a port.
    logical function member(list, b)
        type(handle_type), allocatable, intent(in) :: list(:)
        type(handle_type), intent(in) :: b
        integer :: k

        member = .false.
        if (.not. allocated(list)) return

        do k = 1, size(list)
            if (same_handle(list(k), b)) then
                member = .true.
                return
            end if
        end do
    end function member

    !> The index of BUFFER in the previous buffers of `h', or 0.
    !> Not a port.
    integer function prev_entry(h, buffer) result(k)
        type(handle_type), intent(in) :: h, buffer
        integer :: i

        k = 0
        if (.not. allocated(windows(h%index)%prev_buffers)) return

        do i = 1, size(windows(h%index)%prev_buffers)
            if (same_handle(windows(h%index)%prev_buffers(i)%buffer, buffer)) then
                k = i
                return
            end if
        end do
    end function prev_entry

    !> In WINDOW switch to previous buffer.
    !> Return the buffer switched to, nil if no suitable buffer could be
    !> found.
    !>
    !> Optional argument BURY-OR-KILL non-nil means the buffer currently
    !> shown in WINDOW is about to be buried or killed and consequently
    !> shall not be switched to in future invocations of this command.
    !>
    !> As a special case, if BURY-OR-KILL equals `append', this means to
    !> move the buffer to the end of WINDOW's previous buffers list so a
    !> future invocation of `switch_to_prev_buffer' less likely switches
    !> to it.  Port of window.el:switch-to-prev-buffer
    !> (`switch_to_prev_buffer_skip' nil).
    module function switch_to_prev_buffer(window, bury_or_kill, status) result(new_buffer)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: bury_or_kill
        type(status_type), intent(out) :: status
        type(handle_type) :: new_buffer
        type(handle_type) :: h, old_buffer, b
        type(handle_type), allocatable :: next_buffers(:), list(:)
        type(prev_buffer_type) :: entry
        integer :: k, e
        logical :: found

        status = status_type()
        new_buffer = null_handle()
        h = window
        old_buffer = windows(h%index)%buffer

        ! Save this since it's destroyed by `set_window_buffer'.
        if (allocated(windows(h%index)%next_buffers)) then
            next_buffers = windows(h%index)%next_buffers
        else
            allocate (next_buffers(0))
        end if

        if (windows(h%index)%mini) then
            ! Don't switch in minibuffer window.
            h = selected_frame%selected_window

            if (.not. is_window_live(h)) then
                call set_error(status, FM_GENERIC_ERROR, "Window .false. is a minibuffer window")
                return
            end if
        end if

        if (windows(h%index)%dedicated /= DEDICATED_NONE) then
            ! Don't switch in dedicated window.
            call set_error(status, FM_GENERIC_ERROR, "Window "//window_description(h)// &
                           " is dedicated to buffer #<buffer "//buffer_name_of(old_buffer)//">")
            return
        end if

        found = .false.

        ! Scan WINDOW's previous buffers first, skipping entries of next
        ! buffers.
        if (allocated(windows(h%index)%prev_buffers)) then
            do k = 1, size(windows(h%index)%prev_buffers)
                entry = windows(h%index)%prev_buffers(k)
                b = entry%buffer
                if (same_handle(b, old_buffer)) cycle
                if (.not. is_buffer_live(b)) cycle
                if (bury_or_kill == BK_NONE .and. member(next_buffers, b)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                found = .true.
                exit
            end do
        end if

        ! Scan reverted buffer list of WINDOW's frame next, skipping
        ! entries of next buffers.
        if (.not. found) then
            list = frame_buffer_list()
            if (bury_or_kill == BK_NONE) list = list(size(list):1:-1)

            do k = 1, size(list)
                b = list(k)
                if (.not. is_buffer_live(b) .or. same_handle(b, old_buffer)) cycle
                ! Skip buffers whose names start with a space.
                if (index(buffer_name_of(b), " ") == 1) cycle
                if (bury_or_kill == BK_NONE .and. member(next_buffers, b)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status)
                found = .true.
                exit
            end do
        end if

        if (.not. found .and. bury_or_kill == BK_NONE) then
            ! Scan reverted next buffers last.
            do k = size(next_buffers), 1, -1
                b = next_buffers(k)
                if (.not. is_buffer_live(b) .or. same_handle(b, old_buffer)) cycle
                e = prev_entry(h, b)
                if (e == 0) cycle
                entry = windows(h%index)%prev_buffers(e)
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                exit
            end do
        end if

        if (.not. status%ok()) return

        if (bury_or_kill /= BK_NONE) then
            e = 0
            if (bury_or_kill == BK_APPEND) e = prev_entry(h, old_buffer)

            if (e > 0) then
                ! Keep old-buffer's entry to append it below.
                entry = windows(h%index)%prev_buffers(e)
                windows(h%index)%prev_buffers(e)%start = null_handle()
                windows(h%index)%prev_buffers(e)%point = null_handle()
            end if

            ! Remove `old_buffer' from WINDOW's previous and (restored list
            ! of) next buffers and also from its 'quit-restore' and
            ! 'quit-restore-prev' parameters.
            call unrecord_window_buffer(h, old_buffer, .true.)

            if (e > 0) then
                ! Append old-buffer's entry to list of WINDOW's previous
                ! buffers.
                call append_prev(h, entry)
            end if
        else
            ! Move `old_buffer' to head of WINDOW's restored list of next
            ! buffers.
            call push_front(next_buffers, old_buffer)
            windows(h%index)%next_buffers = next_buffers
        end if
    end function switch_to_prev_buffer

    !> Append ENTRY to the previous buffers of window `h'.  Not a port.
    subroutine append_prev(h, entry)
        type(handle_type), intent(in) :: h
        type(prev_buffer_type), intent(in) :: entry
        type(prev_buffer_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(windows(h%index)%prev_buffers)) allocate (windows(h%index)%prev_buffers(0))
        n = size(windows(h%index)%prev_buffers)
        allocate (grown(n + 1))
        grown(1:n) = windows(h%index)%prev_buffers
        grown(n + 1) = entry
        call move_alloc(grown, windows(h%index)%prev_buffers)
    end subroutine append_prev

    !> (cons b (delq b list)) Not a port.
    subroutine push_front(list, b)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: grown(:)
        integer :: k, n

        n = 0
        allocate (grown(size(list) + 1))
        n = 1
        grown(1) = b

        do k = 1, size(list)
            if (same_handle(list(k), b)) cycle
            n = n + 1
            grown(n) = list(k)
        end do

        list = grown(1:n)
    end subroutine push_front

    !> In WINDOW switch to next buffer.
    !> Return the buffer switched to, nil if no suitable buffer could be
    !> found.  Port of window.el:switch-to-next-buffer
    !> (`switch_to_prev_buffer_skip' nil).
    module function switch_to_next_buffer(window, status) result(new_buffer)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: new_buffer
        type(handle_type) :: h, old_buffer, b
        type(handle_type), allocatable :: next_buffers(:), list(:)
        type(prev_buffer_type) :: entry
        integer :: k, e
        logical :: found

        status = status_type()
        new_buffer = null_handle()
        h = window
        old_buffer = windows(h%index)%buffer

        if (allocated(windows(h%index)%next_buffers)) then
            next_buffers = windows(h%index)%next_buffers
        else
            allocate (next_buffers(0))
        end if

        if (windows(h%index)%mini) then
            ! Don't switch in minibuffer window.
            h = selected_frame%selected_window

            if (.not. is_window_live(h)) then
                call set_error(status, FM_GENERIC_ERROR, "Window .false. is a minibuffer window")
                return
            end if
        end if

        if (windows(h%index)%dedicated /= DEDICATED_NONE) then
            ! Don't switch in dedicated window.
            call set_error(status, FM_GENERIC_ERROR, "Window "//window_description(h)// &
                           " is dedicated to buffer #<buffer "//buffer_name_of(old_buffer)//">")
            return
        end if

        found = .false.

        ! Scan WINDOW's next buffers first.
        do k = 1, size(next_buffers)
            b = next_buffers(k)
            if (.not. is_buffer_live(b) .or. same_handle(b, old_buffer)) cycle
            e = prev_entry(h, b)
            if (e == 0) cycle
            entry = windows(h%index)%prev_buffers(e)
            new_buffer = b
            call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
            found = .true.
            exit
        end do

        ! Scan the buffer list of WINDOW's frame next, skipping previous
        ! buffers entries.
        if (.not. found) then
            list = frame_buffer_list()

            do k = 1, size(list)
                b = list(k)
                if (.not. is_buffer_live(b) .or. same_handle(b, old_buffer)) cycle
                if (index(buffer_name_of(b), " ") == 1) cycle
                if (prev_entry(h, b) > 0) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status)
                found = .true.
                exit
            end do
        end if

        ! Scan WINDOW's reverted previous buffers last (must not use
        ! nreverse here!)
        if (.not. found .and. allocated(windows(h%index)%prev_buffers)) then
            do k = size(windows(h%index)%prev_buffers), 1, -1
                entry = windows(h%index)%prev_buffers(k)
                b = entry%buffer
                if (.not. is_buffer_live(b) .or. same_handle(b, old_buffer)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                exit
            end do
        end if

        ! Remove `new_buffer' from and restore WINDOW's next buffers.
        if (allocated(windows(h%index)%next_buffers)) deallocate (windows(h%index)%next_buffers)
        allocate (windows(h%index)%next_buffers(0))

        do k = 1, size(next_buffers)
            if (same_handle(next_buffers(k), new_buffer)) cycle
            windows(h%index)%next_buffers = [windows(h%index)%next_buffers, next_buffers(k)]
        end do
    end function switch_to_next_buffer

    ! ------------------------------------------------------------------
    ! Deleting and quitting windows
    ! ------------------------------------------------------------------

    !> Whether window `h' can be deleted: not its frame's only window or
    !> the minibuffer window (the frame, Fortmacs's only one, can't be
    !> deleted).  Port of window.el:window-deletable-p.
    logical function is_window_deletable(h)
        type(handle_type), intent(in) :: h

        is_window_deletable = .false.
        if (same_handle(h, selected_frame%root_window)) return
        if (windows(h%index)%mini) return
        is_window_deletable = .true.
    end function is_window_deletable

    !> Delete WINDOW if possible: with DEDICATED_ONLY, only if it is
    !> dedicated.  Return whether WINDOW got deleted.  Port of
    !> window.el:window--delete.
    logical function window_delete(h, dedicated_only) result(deleted)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: dedicated_only
        type(status_type) :: status

        deleted = .false.
        if (dedicated_only .and. windows(h%index)%dedicated == DEDICATED_NONE) return
        if (.not. is_window_deletable(h)) return
        call delete_window(h, status)
        deleted = status%ok()
    end function window_delete

    !> Select WINDOW after having quit another one.  Do not select an
    !> inactive minibuffer window.  Port of
    !> window.el:window--quit-restore-select-window.
    subroutine quit_restore_select_window(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status

        if (.not. is_window_live(h)) return

        if (windows(h%index)%mini) then
            ! The minibuffer window, if the minibuffer is active.
            if (minibuf_level > 0) selected_frame%minibuffer_selected = .true.
            return
        end if

        call select_window(h, status)
    end subroutine quit_restore_select_window

    !> Quit WINDOW and deal with its buffer.
    !> According to information stored in WINDOW's `quit_restore' and
    !> `quit_restore_prev' parameters either (1) delete WINDOW and its
    !> frame, (2) delete WINDOW but leave its frame alone, (3) restore the
    !> buffer previously shown in WINDOW, or (4) make WINDOW display some
    !> other buffer.  In case (3) set any of these parameters to nil if it
    !> has been used to restore the previously shown buffer.
    !>
    !> BURY-OR-KILL tells how to proceed with the buffer of WINDOW: nil
    !> means to not handle the buffer in a particular way; `append' moves it
    !> to the end of WINDOW's previous buffers; `bury' removes it from them
    !> and moves it to the end of the buffer list; `kill' kills it;
    !> `killing' and `burying' are like `kill' and `bury' but mean the
    !> buffer will be killed or buried elsewhere.  Port of
    !> window.el:quit-restore-window (no frames, tabs or side windows).
    module subroutine quit_restore_window(window, bury_or_kill, status)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: bury_or_kill
        type(status_type), intent(out) :: status
        type(handle_type) :: h, buffer, prev_buffer, select_after, b
        type(quit_restore_type) :: qr, qrp, quad_owner
        type(prev_buffer_type) :: entry
        integer :: dedicated, k, e, delta
        logical :: reset_prev, use_prev, use_main, done
        integer(int64) :: start, point

        status = status_type()
        h = window
        if (.not. is_window_live(h)) h = selected_window()
        buffer = windows(h%index)%buffer
        qr = windows(h%index)%quit_restore
        qrp = windows(h%index)%quit_restore_prev
        select_after = qr%selected
        prev_buffer = null_handle()

        if (allocated(windows(h%index)%prev_buffers)) then
            do k = 1, size(windows(h%index)%prev_buffers)
                b = windows(h%index)%prev_buffers(k)%buffer

                if (.not. same_handle(b, buffer) .and. is_buffer_live(b)) then
                    prev_buffer = b
                    exit
                end if
            end do
        end if

        dedicated = windows(h%index)%dedicated
        reset_prev = .false.
        done = .false.

        if (dedicated /= DEDICATED_NONE) then
            ! First try to delete dedicated windows.
            if (window_delete(h, .true.)) then
                call quit_restore_select_window(select_after)
                done = .true.
            end if
        end if

        if (.not. done .and. prev_buffer%index == 0 .and. qr%what == QR_WINDOW .and. &
            .not. same_handle(h, selected_frame%root_window) .and. same_handle(qr%buffer, buffer)) then
            ! Delete WINDOW if possible.
            if (window_delete(h, .false.)) then
                call quit_restore_select_window(select_after)
                done = .true.
            end if
        end if

        if (.not. done) then
            use_prev = .false.
            use_main = .false.

            if (qrp%kind /= QR_NONE .and. qrp%has_quad) then
                if (is_buffer_live(qrp%quad_buffer) .and. same_handle(qrp%buffer, buffer)) use_prev = .true.
            end if

            if (.not. use_prev .and. qr%kind /= QR_NONE .and. qr%has_quad) then
                if (is_buffer_live(qr%quad_buffer) .and. same_handle(qr%buffer, buffer)) use_main = .true.
            end if

            if (use_prev .or. use_main) then
                if (use_prev) then
                    ! Use selected window from quit-restore-prev; we want to
                    ! reset quit-restore-prev only.
                    quad_owner = qrp
                    select_after = qrp%selected
                    reset_prev = .true.
                else
                    quad_owner = qr
                end if

                ! Show another buffer stored in quit-restore(-prev)
                ! parameter.
                if (quad_owner%quad_size >= 0) then
                    if (quad_owner%quad_size /= combined_size(h)) then
                        ! Try to resize WINDOW to its old height but don't
                        ! signal an error.
                        delta = quad_owner%quad_size - combined_size(h)
                        call window_resize(h, delta,.not. is_window_combined(h, .false.), status)
                        status = status_type()
                    end if
                end if

                windows(h%index)%dedicated = DEDICATED_NONE
                ! Restore WINDOW's previous buffer, start and point
                ! position.
                start = quad_owner%quad_start
                point = marker_pos(quad_owner%quad_point)
                call set_window_buffer_start_and_point(h, quad_owner%quad_buffer, status, start, point)
                if (.not. status%ok()) return
                ! Deal with the buffer we just removed from WINDOW.
                e = 0
                if (bury_or_kill == BK_APPEND) e = prev_entry(h, buffer)

                if (any(bury_or_kill == [BK_BURY, BK_BURYING, BK_KILL, BK_KILLING])) then
                    ! Remove buffer from WINDOW's previous and next buffers.
                    call unrecord_window_buffer(h, buffer, .false.)
                end if

                if (e > 0) then
                    ! Append old buffer's entry to list of WINDOW's
                    ! previous buffers so it's less likely to get switched
                    ! to soon but `display_buffer_in_previous_window' can
                    ! nevertheless find it.  (Upstream appends the same
                    ! entry; here a copy with its own markers.)
                    entry%buffer = buffer
                    call copy_marker(windows(h%index)%prev_buffers(e)%start, entry%start, .false., status)
                    call copy_marker(windows(h%index)%prev_buffers(e)%point, entry%point, .false., status)
                    status = status_type()
                    call append_prev(h, entry)
                end if

                ! Reset the quit-restore(-prev) parameter.
                call free_quit_restore(windows(h%index)%quit_restore_prev)
                if (.not. reset_prev) call free_quit_restore(windows(h%index)%quit_restore)

                ! If WINDOW is the selected window and the previously
                ! selected window is still alive, try to select that window.
                if (is_selected(h) .or. .not. any(bury_or_kill == [BK_KILLING, BK_BURYING])) then
                    call quit_restore_select_window(select_after)
                end if
            else
                ! Show some other buffer in WINDOW and leave the
                ! quit-restore(-prev) parameters alone.  Make sure that
                ! WINDOW is no more dedicated.
                windows(h%index)%dedicated = DEDICATED_NONE
                ! Try to switch to a previous buffer.  Delete the window
                ! only if that is not possible.
                b = switch_to_prev_buffer(h, bury_or_kill, status)
                if (.not. status%ok()) return

                if (b%index == 0) then
                    if (window_delete(h, .false.)) continue
                end if
            end if
        end if

        ! Deal with the buffer.
        if (.not. is_buffer_live(buffer)) return

        if (bury_or_kill == BK_KILL) then
            call kill_buffer(buffer, status)
        else if (bury_or_kill == BK_BURY) then
            call bury_buffer_internal(buffer)
        end if
    end subroutine quit_restore_window

    !> Quit WINDOW and bury its buffer.
    !> WINDOW must be a live window and defaults to the selected one.
    !> With prefix argument KILL true, kill the buffer instead of
    !> burying it.
    !>
    !> This calls the function `quit_restore_window' to delete WINDOW or
    !> show some other buffer in it.  Port of window.el:quit-window.
    module subroutine quit_window(kill, status, window)
        logical, intent(in) :: kill
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: h

        h = selected_window()
        if (present(window)) h = window

        if (kill) then
            call quit_restore_window(h, BK_KILL, status)
        else
            call quit_restore_window(h, BK_BURY, status)
        end if
    end subroutine quit_window

    !> Quit WINDOW and bury its buffer.
    !> WINDOW must be a live window and defaults to the selected one.
    !> With prefix argument KILL true, kill the buffer instead of
    !> burying it.  If `quit_window_kill_buffer' is true, perhaps
    !> kill the buffer even if KILL is false; see the doc string of
    !> that variable for the details.
    !>
    !> This calls the function `quit_restore_window' to delete WINDOW or
    !> show some other buffer in it.
    !>
    !> The functions in `quit_window_hook' will be run before doing
    !> anything else.  Port of window.el:quit-window (the command).
    subroutine cmd_quit_window(status)
        type(status_type), intent(out) :: status

        call quit_window(allocated(current_prefix_arg%a), status)
    end subroutine cmd_quit_window

    !> Replace BUFFER_OR_NAME with some other buffer in all windows
    !> showing it.  With the exception of side windows, when a window
    !> showing BUFFER_OR_NAME is dedicated, delete that window.  In any
    !> other case, call `switch_to_prev_buffer' to display some other
    !> buffer in that window.  Remove the buffer from the lists of
    !> previous and next buffers of all windows and remove any
    !> `quit_restore' or `quit_restore_prev' parameters mentioning it.
    !> Port of window.el:replace-buffer-in-windows
    !> (`kill_buffer_quit_windows' nil).
    module subroutine replace_buffer_in_windows(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: b
        type(status_type) :: status
        integer :: k

        if (.not. is_window_valid(selected_frame%root_window)) return
        list = window_list_from()

        do k = 1, size(list)
            if (.not. is_window_live(list(k))) cycle

            if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                if (.not. window_delete(list(k), .true.)) then
                    ! Switch to another buffer in that window.
                    windows(list(k)%index)%dedicated = DEDICATED_NONE
                    b = switch_to_prev_buffer(list(k), BK_KILL, status)

                    if (b%index == 0) then
                        if (window_delete(list(k), .false.)) continue
                    end if
                end if
            end if

            if (is_window_live(list(k))) call unrecord_window_buffer(list(k), buffer, .true.)
        end do

        ! The minibuffer window too.
        if (is_window_live(selected_frame%minibuffer_window)) then
            call unrecord_window_buffer(selected_frame%minibuffer_window, buffer, .true.)
        end if
    end subroutine replace_buffer_in_windows

    module subroutine quit_windows_on(buffer, kill, status)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: kill
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        type(status_type) :: st
        integer :: k

        status = status_type()
        if (.not. is_window_valid(selected_frame%root_window)) return
        list = window_list_from()

        do k = 1, size(list)
            if (.not. is_window_live(list(k))) cycle

            if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                if (kill) then
                    call quit_restore_window(list(k), BK_KILLING, st)
                else
                    call quit_restore_window(list(k), BK_BURYING, st)
                end if
            end if

            ! Unrecord BUFFER in this window.
            if (is_window_live(list(k))) call unrecord_window_buffer(list(k), buffer, .true.)
        end do

        ! Deal with BUFFER-OR-NAME.
        if (.not. is_buffer_live(buffer)) return

        if (kill) then
            call kill_buffer(buffer, status)
        else
            call bury_buffer_internal(buffer)
        end if
    end subroutine quit_windows_on

    !> Put BUFFER_OR_NAME at the end of the list of all buffers.
    !> There it is the least likely candidate for `other_buffer' to
    !> return; thus, the least likely buffer for \[switch_to_buffer] to
    !> select by default.
    !>
    !> If BUFFER_OR_NAME is omitted, bury the current buffer.
    !> Also, if BUFFER_OR_NAME is omitted, remove the current
    !> buffer from the selected window if it is displayed there.  Port of
    !> window.el:bury-buffer.
    module subroutine bury_buffer(status, buffer)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(handle_type) :: b, sw

        status = status_type()

        if (present(buffer)) then
            b = buffer
        else
            b = current_buffer()
        end if

        ! If `buffer_or_name' is not on the selected frame we unrecord it
        ! although it's not "here" (call it a feature).
        sw = selected_window()
        call bury_buffer_internal(b)

        ! Handle case where `buffer_or_name' is nil and the current buffer
        ! is shown in the selected window.
        if (.not. present(buffer) .and. same_handle(windows(sw%index)%buffer, b)) then
            if (windows(sw%index)%mini) return

            if (.not. window_delete(sw, .true.)) then
                ! Switch to another buffer in window.
                windows(sw%index)%dedicated = DEDICATED_NONE
                b = switch_to_prev_buffer(sw, BK_BURY, status)
            end if
        end if
    end subroutine bury_buffer

    !> Put BUFFER_OR_NAME at the end of the list of all buffers.
    !> There it is the least likely candidate for `other_buffer' to
    !> return; thus, the least likely buffer for \[switch_to_buffer] to
    !> select by default.
    !>
    !> You can specify a buffer name as BUFFER_OR_NAME, or an actual
    !> buffer object.  If BUFFER_OR_NAME is omitted, bury the
    !> current buffer.  Also, if BUFFER_OR_NAME is omitted,
    !> remove the current buffer from the selected window if it is
    !> displayed there.  Port of window.el:bury-buffer (the command).
    subroutine cmd_bury_buffer(status)
        type(status_type), intent(out) :: status

        call bury_buffer(status)
    end subroutine cmd_bury_buffer

    !> In selected window switch to ARGth previous buffer.
    !> Call `switch_to_prev_buffer' unless the selected window is the
    !> minibuffer window or is dedicated to its buffer.  Port of
    !> window.el:previous-buffer (`switch_to_prev_buffer_skip' nil).
    subroutine cmd_previous_buffer(status)
        type(status_type), intent(out) :: status

        call prev_or_next_buffer(.true., status)
    end subroutine cmd_previous_buffer

    !> In selected window switch to ARGth next buffer.
    !> Call `switch_to_next_buffer' unless the selected window is the
    !> minibuffer window or is dedicated to its buffer.  Port of
    !> window.el:next-buffer.
    subroutine cmd_next_buffer(status)
        type(status_type), intent(out) :: status

        call prev_or_next_buffer(.false., status)
    end subroutine cmd_next_buffer

    !> The common part of `previous_buffer' and `next_buffer' (called
    !> interactively; the exception for keyboard macros is deferred).
    !> Port of window.el:previous-buffer and window.el:next-buffer (their
    !> common part).
    subroutine prev_or_next_buffer(previous, status)
        logical, intent(in) :: previous
        type(status_type), intent(out) :: status
        type(handle_type) :: sw, b
        integer(int64) :: arg, k

        status = status_type()
        sw = selected_window()

        if (windows(sw%index)%mini) then
            call set_error(status, FM_USER_ERROR, "Cannot switch buffers in minibuffer window")
            return
        end if

        if (windows(sw%index)%dedicated == DEDICATED_STRONG) then
            call set_error(status, FM_USER_ERROR, "Window is strongly dedicated to its buffer")
            return
        end if

        ! (interactive "p"): dotimes does nothing for ARG <= 0.
        arg = prefix_numeric_value(current_prefix_arg)

        do k = 1, arg
            if (previous) then
                b = switch_to_prev_buffer(sw, BK_NONE, status)
                if (.not. status%ok()) return

                if (b%index == 0) then
                    call set_error(status, FM_USER_ERROR, "No previous buffer")
                    return
                end if
            else
                b = switch_to_next_buffer(sw, status)
                if (.not. status%ok()) return

                if (b%index == 0) then
                    call set_error(status, FM_USER_ERROR, "No next buffer")
                    return
                end if
            end if
        end do
    end subroutine prev_or_next_buffer

    ! ------------------------------------------------------------------
    ! display-buffer
    ! ------------------------------------------------------------------

    !> Record information for window used by `display_buffer': install
    !> or update the `quit_restore' parameter of WINDOW (or its
    !> `quit_restore_prev' parameter, when WINDOW is reused for another
    !> buffer while it has one).  TYPE is QR_OTHER for `reuse' or
    !> QR_WINDOW for a new window.  Port of
    !> window.el:display-buffer-record-window.
    module subroutine display_buffer_record_window(type, h, buffer)
        integer, intent(in) :: type
        type(handle_type), intent(in) :: h, buffer
        type(quit_restore_type) :: q
        type(handle_type) :: sw
        type(status_type) :: status

        sw = selected_window()

        associate (w => windows(h%index))
            if (type == QR_OTHER) then
                if (same_handle(w%buffer, buffer)) then
                    ! WINDOW shows BUFFER already.  Update WINDOW's
                    ! quit-restore parameter, if any.
                    if (w%quit_restore%kind /= QR_NONE) then
                        w%quit_restore%kind = QR_SAME

                        ! The selected-window might have changed in
                        ! between.
                        if (.not. same_handle(h, sw) .and. .not. same_handle(h, w%quit_restore%selected)) then
                            w%quit_restore%selected = sw
                        end if
                    end if
                else
                    ! WINDOW shows another buffer: a quadruple of its
                    ! buffer, start, point and height.
                    q%kind = QR_OTHER
                    q%has_quad = .true.
                    q%quad_buffer = w%buffer
                    q%quad_start = window_start(w)
                    call make_marker(q%quad_point)
                    call set_marker(q%quad_point, point_of(h), status, w%buffer)
                    q%quad_size = combined_size(h)
                    q%selected = sw
                    q%buffer = buffer

                    if (w%quit_restore%kind /= QR_NONE) then
                        call free_quit_restore(w%quit_restore_prev)
                        w%quit_restore_prev = q
                    else
                        w%quit_restore = q
                    end if
                end if
            else
                ! WINDOW has been created on an existing frame.
                call free_quit_restore(w%quit_restore)
                w%quit_restore%kind = QR_WINDOW
                w%quit_restore%what = QR_WINDOW
                w%quit_restore%selected = sw
                w%quit_restore%buffer = buffer
            end if
        end associate
    end subroutine display_buffer_record_window

    !> Display BUFFER in WINDOW: TYPE is QR_OTHER when WINDOW existed
    !> already (`reuse'), QR_WINDOW when it is new.  Unless it shows
    !> BUFFER already or is reused, WINDOW is dedicated as
    !> `display_buffer_mark_dedicated' says; a new window gets the height
    !> TABLE's `window_height' function gives it.  Return WINDOW if BUFFER
    !> and WINDOW are live.  Port of window.el:window--display-buffer.
    function window_display_buffer(buffer, h, type, status, table) result(window)
        type(handle_type), intent(in) :: buffer, h
        integer, intent(in) :: type
        type(status_type), intent(out) :: status
        type(display_action_table_type), intent(in) :: table
        type(handle_type) :: window

        status = status_type()
        window = null_handle()
        if (.not. is_buffer_live(buffer) .or. .not. is_window_live(h)) return
        call display_buffer_record_window(type, h, buffer)

        if (.not. same_handle(buffer, windows(h%index)%buffer)) then
            ! Unless WINDOW already shows BUFFER reset its dedicated flag.
            windows(h%index)%dedicated = DEDICATED_NONE
            call set_window_buffer(h, buffer, status)
            if (.not. status%ok()) return
        end if

        ! Maybe dedicate WINDOW to BUFFER if asked for: not if it is
        ! dedicated because it shows BUFFER already or it is reused.
        if (windows(h%index)%dedicated == DEDICATED_NONE .and. type /= QR_OTHER) then
            windows(h%index)%dedicated = display_buffer_mark_dedicated
        end if

        if (type == QR_WINDOW) then
            if (allocated(windows(h%index)%prev_buffers)) call clear_prev_buffers(h)
        end if

        if (associated(table%body_function)) call run_body_function(table, h)

        ! A window that never showed another buffer but BUFFER ever since
        ! it was created on an existing frame: adjust its height if asked
        ! for.
        if (type == QR_WINDOW .or. (windows(h%index)%quit_restore%kind == QR_SAME .and. &
                                    windows(h%index)%quit_restore%what == QR_WINDOW)) then
            if (associated(table%window_height)) call table%window_height(h)
        end if

        window = h
    end function window_display_buffer

    !> Call the `body_function' of TABLE with window H, with
    !> `inhibit_read_only' and `inhibit_modification_hooks' true.
    !> Not a port.
    subroutine run_body_function(table, h)
        type(display_action_table_type), intent(in) :: table
        type(handle_type), intent(in) :: h
        type(value_type) :: saved
        type(status_type) :: st
        logical :: saved_hooks

        saved = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
        saved_hooks = inhibit_modification_hooks%value
        inhibit_modification_hooks = .true.
        call table%body_function(h)
        inhibit_modification_hooks = saved_hooks
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine run_body_function

    !> (set-window-prev-buffers h nil) Not a port.
    subroutine clear_prev_buffers(h)
        type(handle_type), intent(in) :: h
        integer :: k

        do k = 1, size(windows(h%index)%prev_buffers)
            call free_prev_buffer(windows(h%index)%prev_buffers(k))
        end do

        deallocate (windows(h%index)%prev_buffers)
        allocate (windows(h%index)%prev_buffers(0))
    end subroutine clear_prev_buffers

    !> Display BUFFER in the selected window, unless INHIBIT_SAME_WINDOW,
    !> the selected window is a minibuffer window or it is dedicated.
    !> Port of window.el:display-buffer-same-window.
    function display_buffer_same_window(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw

        status = status_type()
        window = null_handle()
        sw = selected_window()

        if (table%inhibit_same_window .or. windows(sw%index)%mini .or. windows(sw%index)%dedicated /= DEDICATED_NONE) then
            return
        end if

        window = window_display_buffer(buffer, sw, QR_OTHER, status, table)
    end function display_buffer_same_window

    !> Return a window that is already displaying BUFFER, preferably the
    !> selected one (unless INHIBIT_SAME_WINDOW).  Port of
    !> window.el:display-buffer-reuse-window.
    function display_buffer_reuse_window(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer :: k

        status = status_type()
        window = null_handle()
        sw = selected_window()

        ! Avoid calling 'get-buffer-window-list' if the selected window
        ! already shows BUFFER and can be used.
        if (same_handle(windows(sw%index)%buffer, buffer) .and. .not. table%inhibit_same_window) then
            window = sw
        else
            list = window_list_from()

            do k = 1, size(list)
                if (table%inhibit_same_window .and. same_handle(list(k), sw)) cycle

                if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                    window = list(k)
                    exit
                end if
            end do
        end if

        if (is_window_live(window)) window = window_display_buffer(buffer, window, QR_OTHER, status, table)
    end function display_buffer_reuse_window

    !> Display BUFFER by popping up a new window: split the largest or
    !> the least recently used window.  Port of
    !> window.el:display-buffer-pop-up-window.
    function display_buffer_pop_up_window(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        status = status_type()
        ! Attempt to split largest or least recently used window.
        window = try_to_split_window(get_largest_window(.true., .false.))
        if (.not. is_window_live(window)) window = try_to_split_window(get_lru_window(.true., .false.))

        if (is_window_live(window)) then
            window = window_display_buffer(buffer, window, QR_WINDOW, status, table)
        else
            window = null_handle()
        end if
    end function display_buffer_pop_up_window

    !> Display BUFFER in a window previously showing it: such a window
    !> other than the selected one, else the selected one (unless
    !> INHIBIT_SAME_WINDOW).  A dedicated window is not usable.  Port of
    !> window.el:display-buffer-in-previous-window (no `previous_window'
    !> entry).
    function display_buffer_in_previous_window(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer :: k

        status = status_type()
        window = null_handle()
        best = null_handle()
        second_best = null_handle()
        sw = selected_window()
        list = window_list_from(frame_first_window())

        do k = 1, size(list)
            if (prev_entry(list(k), buffer) == 0) cycle
            if (windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle

            if (same_handle(list(k), sw)) then
                if (.not. table%inhibit_same_window) second_best = list(k)
            else
                best = list(k)
                exit
            end if
        end do

        if (is_window_live(best)) then
            window = window_display_buffer(buffer, best, QR_OTHER, status, table)
        else if (is_window_live(second_best)) then
            window = window_display_buffer(buffer, second_best, QR_OTHER, status, table)
        end if
    end function display_buffer_in_previous_window

    !> The frame's first window (the top left one).  Port of
    !> window.c:Fframe_first_window.
    function frame_first_window() result(h)
        type(handle_type) :: h
        type(handle_type), allocatable :: leaves(:)

        call window_leaves(selected_frame%root_window, leaves)
        h = leaves(1)
    end function frame_first_window

    !> Return the least recently used window for display-buffer-use-
    !> some-window: not dedicated, preferring a full-width one that is not
    !> the selected window.  Port of window.el:display-buffer--lru-window
    !> (with `window_min_width' `full_width').
    function display_buffer_lru_window(inhibit_same_window) result(window)
        logical, intent(in) :: inhibit_same_window
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: time, best_time, second_best_time
        integer :: k

        best = null_handle()
        second_best = null_handle()
        best_time = -1
        second_best_time = -1
        sw = selected_window()
        list = window_list_from()

        do k = 1, size(list)
            if (windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (inhibit_same_window .and. same_handle(list(k), sw)) cycle
            time = windows(list(k)%index)%use_time

            if (same_handle(list(k), sw) .or. .not. is_window_full_width(list(k))) then
                if (second_best_time < 0 .or. time < second_best_time) then
                    second_best_time = time
                    second_best = list(k)
                end if
            else
                if (best_time < 0 .or. time < best_time) then
                    best_time = time
                    best = list(k)
                end if
            end if
        end do

        if (best%index /= 0) then
            window = best
        else
            window = second_best
        end if
    end function display_buffer_lru_window

    !> Display BUFFER in an existing window: the least recently used one,
    !> or one already showing BUFFER, or the largest one.  Port of
    !> window.el:display-buffer-use-some-window (`some_window' nil).
    function display_buffer_use_some_window(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw
        integer :: delta

        status = status_type()
        sw = selected_window()
        ! Reuse an existing window.
        window = display_buffer_lru_window(table%inhibit_same_window)

        if (.not. is_window_live(window)) then
            window = get_buffer_window(buffer)
            if (table%inhibit_same_window .and. same_handle(window, sw)) window = null_handle()
        end if

        if (.not. is_window_live(window)) window = get_largest_window(.false., table%inhibit_same_window)

        if (.not. is_window_live(window)) then
            window = null_handle()
            return
        end if

        associate (q => windows(window%index)%quit_restore)
            if (q%kind /= QR_NONE .and. q%has_quad .and. q%quad_size >= 0) then
                if (q%quad_size > windows(window%index)%total_lines) then
                    delta = q%quad_size - windows(window%index)%total_lines
                    call window_resize(window, delta, .false., status)
                    status = status_type()
                end if
            end if
        end associate

        window = window_display_buffer(buffer, window, QR_OTHER, status, table)
        if (is_window_live(window)) call window_even_window_sizes(window)
    end function display_buffer_use_some_window

    !> Try displaying BUFFER in a window at the bottom of the selected
    !> frame.  This either reuses such a window provided it shows BUFFER
    !> already, splits a window at the bottom of the frame or the frame's
    !> root window, or reuses some window at the bottom of the selected
    !> frame.  Port of window.el:display-buffer-at-bottom.
    function display_buffer_at_bottom(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, bottom_window
        type(handle_type), allocatable :: list(:)
        integer :: k, frame_bottom
        logical :: bottom_window_shows_buffer

        status = status_type()
        window = null_handle()
        bottom_window = null_handle()
        bottom_window_shows_buffer = .false.

        associate (r => windows(selected_frame%root_window%index))
            frame_bottom = r%top_line + r%total_lines
        end associate

        call window_leaves(selected_frame%root_window, list)

        do k = 1, size(list)
            associate (w => windows(list(k)%index))
                ! A window with another one below it.
                if (w%top_line + w%total_lines < frame_bottom) cycle

                if (.not. bottom_window_shows_buffer .and. same_handle(buffer, w%buffer)) then
                    bottom_window_shows_buffer = .true.
                    bottom_window = list(k)
                else if (bottom_window%index == 0) then
                    bottom_window = list(k)
                end if
            end associate
        end do

        if (bottom_window_shows_buffer) then
            window = window_display_buffer(buffer, bottom_window, QR_OTHER, status, table)
            if (window%index /= 0 .or. .not. status%ok()) return
        end if

        ! Split the frame's main window (its root window).
        call split_window(selected_frame%root_window, 0, .false., .false., window, status)

        if (status%ok()) then
            window = window_display_buffer(buffer, window, QR_WINDOW, status, table)
            if (window%index /= 0 .or. .not. status%ok()) return
        else
            status = status_type()
            window = null_handle()
        end if

        if (is_window_live(bottom_window)) then
            if (windows(bottom_window%index)%dedicated == DEDICATED_NONE) then
                window = window_display_buffer(buffer, bottom_window, QR_OTHER, status, table)
            end if
        end if
    end function display_buffer_at_bottom

    !> Try displaying BUFFER in a window below the selected window.
    !> If there is a window below the selected one and that window
    !> already displays BUFFER, use that window.  Otherwise, try to
    !> create a new window below the selected one and show BUFFER there.
    !> If that attempt fails as well and there is a non-dedicated window
    !> below the selected one, use that window.  Port of
    !> window.el:display-buffer-below-selected (no `window_min_height'
    !> entry).
    function display_buffer_below_selected(buffer, table, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw

        status = status_type()
        window = window_in_direction_below()

        if (is_window_live(window)) then
            if (same_handle(buffer, windows(window%index)%buffer)) then
                window = window_display_buffer(buffer, window, QR_OTHER, status, table)
                if (window%index /= 0 .or. .not. status%ok()) return
            end if
        end if

        ! (let ((split-height-threshold 0) split-width-threshold)
        !   (window--try-to-split-window (selected-window) alist))
        sw = selected_window()
        window = null_handle()

        if (is_window_splittable(sw, .false., threshold_zero=.true.)) then
            call split_window_and_copy(sw, .false., window, status)

            if (.not. status%ok()) then
                status = status_type()
                window = null_handle()
            end if
        end if

        if (is_window_live(window)) then
            window = window_display_buffer(buffer, window, QR_WINDOW, status, table)
            if (window%index /= 0 .or. .not. status%ok()) return
        end if

        window = window_in_direction_below()

        if (is_window_live(window)) then
            if (windows(window%index)%dedicated == DEDICATED_NONE) then
                window = window_display_buffer(buffer, window, QR_OTHER, status, table)
                return
            end if
        end if

        window = null_handle()
    end function display_buffer_below_selected

    module function window_in_direction_below(window) result(best)
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: best, w, best_2, old
        type(handle_type), allocatable :: list(:)
        type(status_type) :: st
        integer :: first, last, posn, best_edge, best_edge_2, best_diff_2, diff, k, w_top, w_left

        best = null_handle()
        best_2 = null_handle()

        if (present(window)) then
            w = window
        else
            w = selected_window()
        end if

        if (.not. is_window_live(w)) return
        first = windows(w%index)%top_line
        last = first + windows(w%index)%total_lines
        ! The column of `posn_at_point' of WINDOW's point.
        old = current_buffer()
        call set_buffer(windows(w%index)%buffer, st)
        posn = screen_column_at(w) + windows(w%index)%left_col
        call set_buffer(old, st)

        associate (r => windows(selected_frame%root_window%index), m => windows(selected_frame%minibuffer_window%index))
            best_edge = r%total_lines + m%total_lines
            best_diff_2 = r%total_cols
        end associate

        best_edge_2 = best_edge
        call window_leaves(selected_frame%root_window, list)
        if (minibuf_level > 0) list = [list, selected_frame%minibuffer_window]

        do k = 1, size(list)
            if (same_handle(list(k), w)) cycle
            w_top = windows(list(k)%index)%top_line
            w_left = windows(list(k)%index)%left_col

            if (w_left <= posn .and. posn < w_left + windows(list(k)%index)%total_cols) then
                ! W is below WINDOW and covers POSN.
                if (w_top >= first .and. w_top < best_edge) then
                    best_edge = w_top
                    best = list(k)
                end if
            else if (last <= w_top) then
                ! W is below WINDOW but does not cover POSN
                ! (window--in-direction-2).
                if (w_left > posn) then
                    diff = w_left - posn
                else
                    diff = posn - w_left - windows(list(k)%index)%total_cols
                end if

                if (diff < best_diff_2 .or. (diff == best_diff_2 .and. w_top < best_edge_2)) then
                    best_edge_2 = w_top
                    best_diff_2 = diff
                    best_2 = list(k)
                end if
            end if
        end do

        if (best%index == 0) best = best_2

    contains

        !> Point's column in window H (the current buffer's point if H is
        !> selected, else its own point).
        integer function screen_column_at(h) result(x)
            type(handle_type), intent(in) :: h
            integer(int64) :: opoint

            opoint = buffers(cur)%pt
            call goto_char(point_of(h))
            x = screen_column()
            call goto_char(opoint)
        end function screen_column_at
    end function window_in_direction_below

    module subroutine switch_to_buffer_other_window(buffer, status, norecord)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord
        type(display_action_table_type) :: table
        logical :: saved

        ! (let ((pop-up-windows t)) (pop-to-buffer buffer-or-name t norecord))
        saved = pop_up_windows
        pop_up_windows = .true.
        table%inhibit_same_window = .true.
        call pop_to_buffer(buffer, status, table=table, norecord=norecord)
        pop_up_windows = saved
    end subroutine switch_to_buffer_other_window

    !> Try action function ACTION for BUFFER with TABLE.  Not a port.
    function display_buffer_action(action, buffer, table, status) result(window)
        integer, intent(in) :: action
        type(handle_type), intent(in) :: buffer
        type(display_action_table_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        status = status_type()
        window = null_handle()

        select case (action)
        case (DISPLAY_ACTION_SAME_WINDOW)
            window = display_buffer_same_window(buffer, table, status)
        case (DISPLAY_ACTION_MAYBE_SAME_WINDOW)
            ! `same_window_p' is false for any buffer with the default
            ! `same_window_buffer_names' and `same_window_regexps'.
            continue
        case (DISPLAY_ACTION_REUSE_WINDOW)
            window = display_buffer_reuse_window(buffer, table, status)
        case (DISPLAY_ACTION_POP_UP_WINDOW)
            window = display_buffer_pop_up_window(buffer, table, status)
        case (DISPLAY_ACTION_MAYBE_POP_UP_WINDOW)
            if (pop_up_windows) window = display_buffer_pop_up_window(buffer, table, status)
        case (DISPLAY_ACTION_IN_PREVIOUS_WINDOW)
            window = display_buffer_in_previous_window(buffer, table, status)
        case (DISPLAY_ACTION_USE_SOME_WINDOW)
            window = display_buffer_use_some_window(buffer, table, status)
        case (DISPLAY_ACTION_AT_BOTTOM)
            window = display_buffer_at_bottom(buffer, table, status)
        case (DISPLAY_ACTION_BELOW_SELECTED)
            window = display_buffer_below_selected(buffer, table, status)
        case default
            continue
        end select
    end function display_buffer_action

    !> Display BUFFER in some window and return that window, or the null
    !> handle if no window could be found.  The action functions ACTIONS
    !> are tried in turn with TABLE, then those of
    !> `display_buffer_fallback_action':
    !> `display_buffer__maybe_same_window', `display_buffer_reuse_window',
    !> `display_buffer__maybe_pop_up_window',
    !> `display_buffer_in_previous_window' and
    !> `display_buffer_use_some_window', after those of
    !> `display_buffer_overriding_action'.  Port of window.el:display-buffer
    !> (one frame; `display_buffer_table' and `display_buffer_base_action'
    !> are deferred: they are empty).
    module subroutine display_buffer(buffer, window, status, actions, table)
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: actions(:)
        type(display_action_table_type), intent(in), optional :: table
        integer, parameter :: fallback(5) = [DISPLAY_ACTION_MAYBE_SAME_WINDOW, DISPLAY_ACTION_REUSE_WINDOW, &
                                             DISPLAY_ACTION_MAYBE_POP_UP_WINDOW, DISPLAY_ACTION_IN_PREVIOUS_WINDOW, &
                                             DISPLAY_ACTION_USE_SOME_WINDOW]
        type(display_action_table_type) :: al
        integer :: k

        status = status_type()
        window = null_handle()

        if (.not. is_buffer_live(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid buffer")
            return
        end if

        call ensure_frame_windows()
        if (present(table)) al = table

        ! `display_buffer_overriding_action' comes first.
        if (display_buffer_overriding_action%has_inhibit_same_window) then
            al%inhibit_same_window = display_buffer_overriding_action%inhibit_same_window
        end if

        if (allocated(display_buffer_overriding_action%actions)) then
            do k = 1, size(display_buffer_overriding_action%actions)
                window = display_buffer_action(display_buffer_overriding_action%actions(k), buffer, al, status)
                if (.not. status%ok() .or. window%index /= 0) return
            end do
        end if

        if (present(actions)) then
            do k = 1, size(actions)
                window = display_buffer_action(actions(k), buffer, al, status)
                if (.not. status%ok() .or. window%index /= 0) return
            end do
        end if

        do k = 1, size(fallback)
            window = display_buffer_action(fallback(k), buffer, al, status)
            if (.not. status%ok() .or. window%index /= 0) return
        end do
    end subroutine display_buffer

    !> Display buffer BUFFER and select its window.  This uses
    !> `display_buffer' with ACTIONS and TABLE and then selects the window;
    !> unless NORECORD, BUFFER becomes the most recently selected buffer.
    !> If no window shows it, BUFFER becomes current.  Port of
    !> window.el:pop-to-buffer (one frame).
    module subroutine pop_to_buffer(buffer, status, actions, table, norecord)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: actions(:)
        type(display_action_table_type), intent(in), optional :: table
        logical, intent(in), optional :: norecord
        type(handle_type) :: window

        call display_buffer(buffer, window, status, actions, table)
        if (.not. status%ok()) return

        if (.not. is_window_live(window)) then
            call set_buffer(buffer, status)
            return
        end if

        ! Make sure the window is selected.
        call select_window(window, status, norecord)
    end subroutine pop_to_buffer

    !> Select buffer BUFFER in some window, preferably the same one.
    !> Port of window.el:pop-to-buffer-same-window
    !> (display-buffer--same-window-action).
    module subroutine pop_to_buffer_same_window(buffer, status, norecord)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord

        call pop_to_buffer(buffer, status, [DISPLAY_ACTION_SAME_WINDOW], norecord=norecord)
    end subroutine pop_to_buffer_same_window

    module subroutine fit_window_to_buffer(window, max_height)
        type(handle_type), intent(in) :: window
        integer, intent(in), optional :: max_height
        type(handle_type) :: sub, old
        type(status_type) :: status
        integer :: total_height, min_height, max_h, max_delta, height
        integer(int64) :: moved, opoint

        if (.not. is_window_live(window)) return
        ! Only a window in a vertical combination is fitted.
        if (.not. is_window_combined(window, .false.)) return
        total_height = windows(window%index)%total_lines
        ! Can't get smaller than `window_min_height'.
        min_height = max(window_min_height, window_min_size(window, .false.))
        ! What the other windows of the combination can give
        ! (window-max-delta with NOUP).
        max_delta = 0
        sub = windows(windows(window%index)%parent%index)%child

        do while (is_window_valid(sub))
            if (.not. same_handle(sub, window)) then
                max_delta = max_delta + max(windows(sub%index)%total_lines - window_min_size(sub, .false.), 0)
            end if

            sub = windows(sub%index)%next
        end do

        max_h = total_height + max_delta
        if (present(max_height)) max_h = min(max_h, max_height)
        ! The screen lines of the whole buffer and the mode line
        ! (window-text-pixel-size from point-min to point-max).
        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, status)
        if (.not. status%ok()) return
        opoint = buffers(cur)%pt
        call goto_char(buffers(cur)%begv)
        height = 1

        do
            call vertical_motion(1_int64, moved, window=windows(window%index))
            if (moved == 0) exit
            height = height + 1
        end do

        call goto_char(opoint)
        call set_buffer(old, status)
        height = height + 1
        height = max(min_height, min(max_h, height))
        if (height /= total_height) call window_resize(window, height - total_height, .false., status)
    end subroutine fit_window_to_buffer

    module subroutine shrink_window_if_larger_than_buffer(window)
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w
        type(status_type) :: status
        integer(int64) :: b

        if (present(window)) then
            w = window
        else
            w = selected_window()
        end if

        if (.not. is_window_live(w)) return
        ! Make sure that WINDOW is vertically combined and `point-min' is
        ! visible (for whatever reason that's needed).  The remaining issues
        ! should be taken care of by `fit-window-to-buffer'.
        if (.not. is_window_combined(w, .false.)) return
        call resolve_buffer(windows(w%index)%buffer, b, status)
        if (.not. status%ok()) return
        if (window_start(windows(w%index)) > buffers(b)%begv) return
        call fit_window_to_buffer(w, windows(w%index)%total_lines)
    end subroutine shrink_window_if_larger_than_buffer

    !> Show temporary buffer BUFFER in a window: unmodified, read-only and
    !> at its beginning.  Return the window showing BUFFER.  Port of
    !> window.el:temp-buffer-window-show (no `temp_buffer_resize_mode').
    module subroutine temp_buffer_window_show(buffer, window, status, actions, table)
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: actions(:)
        type(display_action_table_type), intent(in), optional :: table
        type(handle_type) :: saved

        window = null_handle()
        saved = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call restore_buffer_modified(.false.)
        call set_buffer_read_only(buffer, .true., status)
        call goto_char(buffers(cur)%begv)
        call display_buffer(buffer, window, status, actions, table)
        if (is_buffer_live(saved)) call set_buffer(saved, status)
        if (.not. is_window_live(window)) return
        minibuffer_scroll_window = window
        windows(window%index)%hscroll = 0
    end subroutine temp_buffer_window_show

    !> Return a window currently displaying BUFFER_OR_NAME, or nil if none:
    !> the selected window if it does, else the first in the cyclic order
    !> from it.  Port of window.c:Fget_buffer_window.
    module function get_buffer_window(buffer) result(window)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: k

        window = null_handle()
        list = window_list_from()

        do k = 1, size(list)
            if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                window = list(k)
                return
            end if
        end do
    end function get_buffer_window

    ! ------------------------------------------------------------------
    ! Choosing and splitting windows
    ! ------------------------------------------------------------------

    !> Return the least recently used window on the frame: a full-width
    !> window if possible, avoiding the selected window if possible (never
    !> it with NOT_SELECTED).  A dedicated window is never a candidate
    !> unless DEDICATED.  Port of window.el:get-lru-window.
    module function get_lru_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: time, best_time, second_best_time
        integer :: k

        best = null_handle()
        second_best = null_handle()
        best_time = -1
        second_best_time = -1
        sw = selected_window()
        list = window_list_from()

        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle
            time = windows(list(k)%index)%use_time

            if (same_handle(list(k), sw) .or. .not. is_window_full_width(list(k))) then
                if (second_best_time < 0 .or. time < second_best_time) then
                    second_best_time = time
                    second_best = list(k)
                end if
            else
                if (best_time < 0 .or. time < best_time) then
                    best_time = time
                    best = list(k)
                end if
            end if
        end do

        if (best%index /= 0) then
            window = best
        else
            window = second_best
        end if
    end function get_lru_window

    !> Return the most recently used window on the frame.  Port of
    !> window.el:get-mru-window.
    module function get_mru_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: best_time
        integer :: k

        window = null_handle()
        best_time = -1
        sw = selected_window()
        list = window_list_from()

        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle

            if (best_time < 0 .or. windows(list(k)%index)%use_time > best_time) then
                best_time = windows(list(k)%index)%use_time
                window = list(k)
            end if
        end do
    end function get_mru_window

    !> Return the largest window on the frame.  A dedicated window is never
    !> a candidate unless DEDICATED; NOT_SELECTED means never return the
    !> selected window.  Port of window.el:get-largest-window.
    module function get_largest_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer :: k, area, best_size

        window = null_handle()
        best_size = 0
        sw = selected_window()
        list = window_list_from()

        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle
            area = windows(list(k)%index)%total_lines*windows(list(k)%index)%total_cols

            if (area > best_size) then
                best_size = area
                window = list(k)
            end if
        end do
    end function get_largest_window

    !> Return true if `split_window_sensibly' may split WINDOW: one at
    !> least `split_height_threshold' lines high and twice
    !> `window_min_height' (or 2), or with HORIZONTAL at least
    !> `split_width_threshold' columns wide and twice `window_min_width'
    !> (or 2).  THRESHOLD_ZERO stands for `split_height_threshold' bound
    !> to 0.  Port of window.el:window-splittable-p.
    logical function is_window_splittable(h, horizontal, threshold_zero)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        logical, intent(in), optional :: threshold_zero
        integer :: threshold

        is_window_splittable = .false.
        if (.not. is_window_live(h)) return

        if (horizontal) then
            is_window_splittable = body_cols(h) >= max(split_width_threshold, 2*max(window_min_width, 2))
        else
            threshold = split_height_threshold

            if (present(threshold_zero)) then
                if (threshold_zero) threshold = 0
            end if

            ! The window has a mode line.
            is_window_splittable = windows(h%index)%total_lines >= max(threshold, 2*max(window_min_height, 2))
        end if
    end function is_window_splittable

    !> Split WINDOW below (or with HORIZONTAL to the right) and return the
    !> new window, which gets WINDOW's `quit_restore' parameter.  Port of
    !> window.el:split-window-below and split-window-right (no SIZE,
    !> `split_window_keep_point' t).
    module subroutine split_window_and_copy(h, horizontal, new, status)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status

        call split_window(h, 0, .false., horizontal, new, status)
        if (.not. status%ok()) return
        ! Always copy quit-restore parameter in interactive use.
        call copy_quit_restore(windows(h%index)%quit_restore, windows(new%index)%quit_restore)
    end subroutine split_window_and_copy

    !> TO := FROM, with a marker of its own.
    module subroutine copy_quit_restore(from, to)
        type(quit_restore_type), intent(in) :: from
        type(quit_restore_type), intent(inout) :: to
        type(status_type) :: status
        integer(int64) :: pos

        if (from%kind == QR_NONE) return
        call free_quit_restore(to)
        to = from
        to%quad_point = null_handle()

        if (from%quad_point%index /= 0) then
            pos = marker_pos(from%quad_point)
            call make_marker(to%quad_point)
            call set_marker(to%quad_point, pos, status, from%quad_buffer)
        end if
    end subroutine copy_quit_restore

    !> Whether the frame is wider than it is tall (characters being about
    !> twice as tall as wide).  Port of window.el:window--frame-landscape-p.
    logical function is_frame_landscape()
        associate (r => windows(selected_frame%root_window%index), m => windows(selected_frame%minibuffer_window%index))
            is_frame_landscape = r%total_cols > 2*(r%total_lines + m%total_lines)
        end associate
    end function is_frame_landscape

    !> Split WINDOW in a way suitable for `display_buffer'.  Try the
    !> directions in the order `split_window_preferred_direction' says,
    !> each if `is_window_splittable' allows it; if both fail and WINDOW is
    !> the only window on its frame (or all others are dedicated), split
    !> it vertically disregarding `split_height_threshold'.  Return the new
    !> window or the null handle.  Port of window.el:split-window-sensibly.
    function split_window_sensibly(h) result(new)
        type(handle_type), intent(in) :: h
        type(handle_type) :: new
        type(handle_type), allocatable :: list(:)
        logical :: horizontal_first, others
        integer :: k

        horizontal_first = split_window_preferred_direction == SPLIT_HORIZONTAL .or. &
                           (split_window_preferred_direction == SPLIT_LONGEST .and. is_frame_landscape())

        if (horizontal_first) then
            new = try_split(h, .true.)
            if (new%index == 0) new = try_split(h, .false.)
        else
            new = try_split(h, .false.)
            if (new%index == 0) new = try_split(h, .true.)
        end if

        if (new%index /= 0) return
        others = .false.

        if (.not. same_handle(h, selected_frame%root_window)) then
            list = window_list_from()

            do k = 1, size(list)
                if (same_handle(list(k), h)) cycle
                if (windows(list(k)%index)%dedicated == DEDICATED_NONE) others = .true.
            end do
        end if

        if (others .or. windows(h%index)%mini) return
        if (is_window_splittable(h, .false., threshold_zero=.true.)) new = do_split(h, .false.)

    contains

        function try_split(w, horizontal) result(n)
            type(handle_type), intent(in) :: w
            logical, intent(in) :: horizontal
            type(handle_type) :: n

            n = null_handle()
            if (is_window_splittable(w, horizontal)) n = do_split(w, horizontal)
        end function try_split

        function do_split(w, horizontal) result(n)
            type(handle_type), intent(in) :: w
            logical, intent(in) :: horizontal
            type(handle_type) :: n
            type(status_type) :: status

            call split_window_and_copy(w, horizontal, n, status)
            if (.not. status%ok()) n = null_handle()
        end function do_split
    end function split_window_sensibly

    !> Try to split WINDOW: return the new window if
    !> `split_window_sensibly' made one, the null handle otherwise.
    !> Port of window.el:window--try-to-split-window.
    function try_to_split_window(h) result(new)
        type(handle_type), intent(in) :: h
        type(handle_type) :: new

        new = null_handle()
        if (.not. is_window_live(h)) return
        new = split_window_sensibly(h)
        if (.not. is_window_live(new)) new = null_handle()
    end function try_to_split_window

    !> Even sizes of WINDOW and selected window, if these windows are the
    !> only children of their parent and the selected window is larger
    !> than WINDOW.  Port of window.el:window--even-window-sizes
    !> (`even_window_sizes' t).
    subroutine window_even_window_sizes(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: sw, parent
        type(status_type) :: status

        if (.not. even_window_sizes) return
        sw = selected_window()
        parent = windows(h%index)%parent
        if (.not. is_window_valid(parent)) return
        if (child_count(parent) /= 2) return
        if (.not. same_handle(windows(sw%index)%parent, parent)) return

        if (is_window_combined(h, .true.) .and. windows(sw%index)%total_cols > windows(h%index)%total_cols) then
            call window_resize(sw, (windows(h%index)%total_cols - windows(sw%index)%total_cols)/2, .true., status)
        else if (is_window_combined(h, .false.) .and. windows(sw%index)%total_lines > windows(h%index)%total_lines) then
            call window_resize(sw, (windows(h%index)%total_lines - windows(sw%index)%total_lines)/2, .false., status)
        end if
    end subroutine window_even_window_sizes

    !> The number of children of internal window `h'.  Not a port.
    integer function child_count(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: c

        child_count = 0
        c = windows(h%index)%child

        do while (is_window_valid(c))
            child_count = child_count + 1
            c = windows(c%index)%next
        end do
    end function child_count

    !> Resize window `h' by DELTA lines (or with HORIZONTAL columns),
    !> taking the space from (or giving it to) its next sibling, else its
    !> previous one; each window keeps at least `window_min_height' lines
    !> (or `window_min_width' columns).  Port of window.el:window-resize
    !> for a window with such a sibling (`window_combination_resize' nil).
    module subroutine window_resize(window, delta, horizontal, status)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal
        type(status_type), intent(out) :: status
        type(handle_type) :: sib, h

        status = status_type()
        if (delta == 0) return

        if (.not. is_window_valid(windows(window%index)%parent)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot resize the root window of a frame")
            return
        end if

        ! A window not combined in that direction resizes the combination
        ! it is in (window--resize-this-window of its parent).
        h = window

        do while (.not. is_window_combined(h, horizontal))
            h = windows(h%index)%parent

            if (.not. is_window_valid(windows(h%index)%parent)) then
                call set_error(status, FM_GENERIC_ERROR, "Cannot resize window "//window_description(window))
                return
            end if
        end do

        ! The sibling on the right (below), else on the left (above), gives
        ! or takes DELTA.
        sib = windows(h%index)%next
        if (.not. is_window_valid(sib)) sib = windows(h%index)%prev

        if (size_of(sib) - delta < window_min_size(sib, horizontal) .or. &
            size_of(h) + delta < window_min_size(h, horizontal)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot resize window "//window_description(window))
            return
        end if

        if (same_handle(sib, windows(h%index)%next)) then
            call resize_window_tree(h, windows(h%index)%top_line, windows(h%index)%left_col, &
                                    lines_of(h, delta), cols_of(h, delta), frame_cols())

            if (horizontal) then
                call resize_window_tree(sib, windows(sib%index)%top_line, windows(sib%index)%left_col + delta, &
                                        lines_of(sib, 0), cols_of(sib, -delta), frame_cols())
            else
                call resize_window_tree(sib, windows(sib%index)%top_line + delta, windows(sib%index)%left_col, &
                                        lines_of(sib, -delta), cols_of(sib, 0), frame_cols())
            end if
        else
            call resize_window_tree(sib, windows(sib%index)%top_line, windows(sib%index)%left_col, &
                                    lines_of(sib, -delta), cols_of(sib, -delta), frame_cols())

            if (horizontal) then
                call resize_window_tree(h, windows(h%index)%top_line, windows(h%index)%left_col - delta, &
                                        lines_of(h, 0), cols_of(h, delta), frame_cols())
            else
                call resize_window_tree(h, windows(h%index)%top_line - delta, windows(h%index)%left_col, &
                                        lines_of(h, delta), cols_of(h, 0), frame_cols())
            end if
        end if

    contains

        integer function size_of(w)
            type(handle_type), intent(in) :: w

            if (horizontal) then
                size_of = windows(w%index)%total_cols
            else
                size_of = windows(w%index)%total_lines
            end if
        end function size_of

        integer function lines_of(w, d)
            type(handle_type), intent(in) :: w
            integer, intent(in) :: d

            lines_of = windows(w%index)%total_lines
            if (.not. horizontal) lines_of = lines_of + d
        end function lines_of

        integer function cols_of(w, d)
            type(handle_type), intent(in) :: w
            integer, intent(in) :: d

            cols_of = windows(w%index)%total_cols
            if (horizontal) cols_of = cols_of + d
        end function cols_of

        integer function frame_cols()
            frame_cols = windows(selected_frame%root_window%index)%left_col + &
                         windows(selected_frame%root_window%index)%total_cols
        end function frame_cols
    end subroutine window_resize

    !> Register the commands of this submodule.
    module subroutine syms_of_window_display()
        replace_buffer_in_windows_hook => replace_buffer_in_windows
        commands%quit_window%run => cmd_quit_window
        commands%bury_buffer%run => cmd_bury_buffer
        commands%previous_buffer%run => cmd_previous_buffer
        commands%next_buffer%run => cmd_next_buffer
    end subroutine syms_of_window_display
end submodule window_display_fm
