! window_configuration_mod.f90 -- window configurations.
! Ported from GNU Emacs 31.1 src/window.c (save_window_save,
! Fcurrent_window_configuration, Fset_window_configuration,
! delete_all_child_windows, compare_window_configurations).
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

!> Window configurations: a snapshot of the selected frame's windows
!> (`current_window_configuration') that `set_window_configuration'
!> puts back, reviving windows deleted meanwhile.  Reading from the
!> minibuffer restores the configuration on exit
!> (`read_minibuffer_restore_windows').
!>
!> Deferred: other frames, window parameters (no persistent ones are
!> ported), `window_restore_closed_buffer_windows' other than .false., the
!> frame's focus and `minibuffer_scroll_window'.
submodule(window_mod) window_configuration_mod
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, resolve_buffer, buffer_list, &
                          get_or_make_buffer
    use marker_mod, only: make_marker, destroy_marker, set_marker, set_marker_restricted, marker_position
    use editing_functions_mod, only: goto_char
    use frame_mod, only: selected_frame, frame_lines, frame_top_margin, resize_frame_windows
    use minibuffer_reading_mod, only: is_minibuffer, minibuf_level
    use error_mod, only: set_error, FM_GENERIC_ERROR
    implicit none(type, external)

contains

    !> The number of windows in the tree under `h' and its next siblings.
    !> Port of window.c:count_windows.
    recursive integer function count_windows(h) result(n)
        type(handle_type), intent(in) :: h
        type(handle_type) :: w

        n = 0
        w = h

        do while (is_window_valid(w))
            n = n + 1
            if (is_window_valid(windows(w%index)%child)) n = n + count_windows(windows(w%index)%child)
            w = windows(w%index)%next
        end do
    end function count_windows

    !> Save `h' and its next siblings, with their children, in `saved'
    !> from index `i' on (depth first); `parent' is the index of their
    !> parent.  Return the next free index.  Port of
    !> window.c:save_window_save.
    recursive integer function save_window_save(h, saved, i, parent) result(next)
        type(handle_type), intent(in) :: h
        type(saved_window_type), intent(inout) :: saved(:)
        integer, intent(in) :: i, parent
        type(handle_type) :: w
        type(status_type) :: status
        integer(int64) :: pos, bi
        integer :: prev

        next = i
        prev = 0
        w = h

        do while (is_window_valid(w))
            associate (p => saved(next), x => windows(w%index))
                p%window = w
                p%buffer = null_handle()
                if (.not. is_window_valid(x%child)) p%buffer = x%buffer
                p%left_col = x%left_col
                p%top_line = x%top_line
                p%total_cols = x%total_cols
                p%total_lines = x%total_lines
                p%divider = x%divider
                p%normal_lines = x%normal_lines
                p%normal_cols = x%normal_cols
                p%hscroll = x%hscroll
                p%dedicated = x%dedicated
                p%parent = parent
                p%prev = prev
                x%config_refs = x%config_refs + 1

                if (is_buffer_live(p%buffer)) then
                    ! Save w's value of point in the window
                    ! configuration.  If w is the selected window, then
                    ! get the value of point from the buffer; pointm is
                    ! garbage in the selected window.
                    call make_marker(p%pointm)

                    if (same_window(w, selected_frame%selected_window)) then
                        call resolve_buffer(p%buffer, bi, status)
                        pos = buffers(bi)%pt
                    else
                        pos = window_point(x)
                    end if

                    call set_marker(p%pointm, pos, status, p%buffer)
                    call make_marker(p%start)
                    call set_marker(p%start, window_start(x), status, p%buffer)
                end if
            end associate

            prev = next
            next = next + 1

            if (is_window_valid(windows(w%index)%child)) then
                next = save_window_save(windows(w%index)%child, saved, next, prev)
            end if

            w = windows(w%index)%next
        end do
    end function save_window_save

    module subroutine current_window_configuration(config)
        type(window_configuration_type), intent(out) :: config
        integer :: n

        call ensure_frame_windows_here()
        allocate (config%saved(count_windows(selected_frame%root_window)))
        n = save_window_save(selected_frame%root_window, config%saved, 1, 0)
        config%root_window = selected_frame%root_window
        config%current_window = selected_frame%selected_window
        config%current_buffer = current_buffer()
        config%minibuf_scroll_window = null_handle()
        if (minibuf_level > 0) config%minibuf_scroll_window = minibuffer_scroll_window
        config%frame_cols = selected_frame%text_cols
        config%frame_lines = selected_frame%text_lines
    end subroutine current_window_configuration

    !> The frame has its windows.  Not a port.
    subroutine ensure_frame_windows_here()
        if (.not. is_window_valid(selected_frame%root_window)) then
            call make_window(selected_frame%root_window)
            selected_frame%selected_window = selected_frame%root_window
        end if
    end subroutine ensure_frame_windows_here

    !> Take the windows of the tree under `h' and its next siblings out of
    !> it: each leaf stops showing its buffer, remembering it in
    !> `kept_buffer' for a revival.  The windows are listed in `old'.
    !> Port of window.c:delete_all_child_windows.
    recursive subroutine delete_all_child_windows(h, old, n)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(inout) :: old(:)
        integer, intent(inout) :: n
        type(status_type) :: status

        if (is_window_valid(windows(h%index)%next)) call delete_all_child_windows(windows(h%index)%next, old, n)
        n = n + 1
        old(n) = h

        associate (w => windows(h%index))
            if (is_window_valid(w%child)) then
                call delete_all_child_windows(w%child, old, n)
                w%child = null_handle()
            else if (is_buffer_live(w%buffer)) then
                call unshow_buffer(h)
                if (w%pointm%index /= 0) call destroy_marker(w%pointm, status)
                if (w%start%index /= 0) call destroy_marker(w%start, status)
                w%pointm = null_handle()
                w%start = null_handle()
                ! Since combination limit makes sense for an internal
                ! window only, we use this slot to save the buffer for the
                ! sake of possible resurrection.
                w%kept_buffer = w%buffer
                w%buffer = null_handle()
            end if
        end associate
    end subroutine delete_all_child_windows

    !> Set marker `m' (made if null) restricted to the accessible part of
    !> `buffer' at `pos'.  Not a port.
    subroutine restrict(m, pos, buffer)
        type(handle_type), intent(inout) :: m
        integer(int64), intent(in) :: pos
        type(handle_type), intent(in) :: buffer
        type(status_type) :: status

        if (m%index == 0) call make_marker(m)
        call set_marker_restricted(m, pos, status, buffer)
    end subroutine restrict

    !> The position of marker `m' (1 if it points nowhere).  Not a port.
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: status

        pos = 1
        if (m%index == 0) return
        call marker_position(m, pos, status)
        if (.not. status%ok()) pos = 1
    end function marker_pos

    !> A live buffer other than `buffer' that isn't internal, else
    !> *scratch*.  Port of buffer.c:other_buffer_safely.
    function other_buffer_safely(buffer) result(b)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: b
        type(handle_type), allocatable :: all(:)
        type(status_type) :: status
        integer :: k

        all = buffer_list()

        do k = 1, size(all)
            if (.not. is_buffer_live(all(k))) cycle
            if (same_handle(all(k), buffer)) cycle

            if (len(buffers(all(k)%index)%name) > 0) then
                if (buffers(all(k)%index)%name(1:1) == " ") cycle
            end if

            b = all(k)
            return
        end do

        call get_or_make_buffer("*scratch*", b, status)
    end function other_buffer_safely

    module subroutine set_window_configuration(config)
        type(window_configuration_type), intent(in) :: config
        type(handle_type) :: new_current_buffer, window, sw, b
        type(handle_type), allocatable :: old(:), dead_windows(:)
        type(status_type) :: status
        integer(int64) :: old_point, bi
        integer :: k, n, ndead

        if (.not. allocated(config%saved)) return
        if (size(config%saved) == 0) return
        old_point = -1
        new_current_buffer = config%current_buffer
        sw = selected_frame%selected_window

        if (.not. is_buffer_live(new_current_buffer)) then
            new_current_buffer = null_handle()
        else
            ! Be careful which point of the current buffer is copied into
            ! old_point: the point of the window to be selected.
            call resolve_buffer(new_current_buffer, bi, status)
            ! The point of the window to be selected, if it shows the
            ! buffer and is not the selected window; else the buffer's.
            old_point = buffers(bi)%pt

            if (window_current_shows(config%current_window, new_current_buffer) .and. &
                .not. same_window(sw, config%current_window)) then
                if (.not. same_handle(new_current_buffer, current_buffer())) then
                    old_point = window_point(windows(config%current_window%index))
                else if (window_current_shows(sw, new_current_buffer)) then
                    old_point = window_point(windows(config%current_window%index))
                end if
            end if
        end if

        ! If a window we restore gets another buffer, record the window's
        ! old buffer.
        do k = 1, size(config%saved)
            associate (p => config%saved(k))
                if (.not. is_window_live(p%window)) cycle
                if (.not. is_buffer_live(windows(p%window%index)%buffer)) cycle
                if (same_handle(windows(p%window%index)%buffer, p%buffer)) cycle
                if (.not. is_buffer_live(p%buffer)) cycle
                if (is_minibuffer(p%buffer)) cycle
                call record_window_buffer(p%window)
            end associate
        end do

        ! "Swap out" point from the selected window's buffer into the
        ! window itself.
        if (is_window_live(sw)) then
            if (is_buffer_live(windows(sw%index)%buffer)) then
                call resolve_buffer(windows(sw%index)%buffer, bi, status)
                call set_window_point(windows(sw%index), buffers(bi)%pt, status)
            end if
        end if

        ! Mark all windows now on frame as "deleted".  Restoring the new
        ! configuration "undeletes" any that are in it.
        allocate (old(count_windows(selected_frame%root_window)))
        n = 0

        do k = 1, size(old)
            old(k) = null_handle()
        end do

        if (is_window_valid(selected_frame%root_window)) then
            call delete_all_child_windows(selected_frame%root_window, old, n)
        end if

        do k = 1, n
            windows(old(k)%index)%dead = .true.
            windows(old(k)%index)%parent = null_handle()
            windows(old(k)%index)%next = null_handle()
            windows(old(k)%index)%prev = null_handle()
        end do

        allocate (dead_windows(size(config%saved)))
        ndead = 0

        do k = 1, size(config%saved)
            associate (p => config%saved(k))
                window = p%window

                associate (w => windows(window%index))
                    w%dead = .false.
                    w%next = null_handle()
                    w%child = null_handle()

                    if (p%parent /= 0) then
                        w%parent = config%saved(p%parent)%window
                    else
                        w%parent = null_handle()
                    end if

                    if (p%prev /= 0) then
                        w%prev = config%saved(p%prev)%window
                        windows(w%prev%index)%next = window
                    else
                        w%prev = null_handle()

                        if (is_window_valid(w%parent)) then
                            windows(w%parent%index)%horizontal = &
                                p%total_cols /= windows(w%parent%index)%total_cols
                            windows(w%parent%index)%child = window
                        end if
                    end if

                    ! If we squirreled away the buffer, restore it now.
                    if (w%kept_buffer%index /= 0) then
                        w%buffer = w%kept_buffer
                        w%kept_buffer = null_handle()
                    end if

                    w%left_col = p%left_col
                    w%top_line = p%top_line
                    w%total_cols = p%total_cols
                    w%total_lines = p%total_lines
                    w%divider = p%divider
                    w%normal_lines = p%normal_lines
                    w%normal_cols = p%normal_cols
                    w%hscroll = p%hscroll
                    w%dedicated = p%dedicated
                    w%window_end_valid = .false.
                end associate

                if (is_buffer_live(p%buffer)) then
                    ! If saved buffer is alive, install it.
                    if (.not. same_handle(windows(window%index)%buffer, p%buffer)) then
                        windows(window%index)%buffer = p%buffer
                        call window_discard_buffer_from_window(p%buffer, window, .false.)
                    end if

                    call restrict(windows(window%index)%start, marker_pos(p%start), p%buffer)
                    call restrict(windows(window%index)%pointm, marker_pos(p%pointm), p%buffer)

                    ! Don't restore the location of point in the buffer
                    ! which was current when the configuration was
                    ! recorded.
                    if (.not. same_handle(p%buffer, new_current_buffer) .and. &
                        same_handle(p%buffer, current_buffer())) then
                        call goto_char(window_point(windows(window%index)))
                    end if
                else if (is_buffer_live(windows(window%index)%buffer)) then
                    ! Keep window's old buffer; make sure the markers are
                    ! real.
                    b = windows(window%index)%buffer
                    call resolve_buffer(b, bi, status)

                    if (windows(window%index)%start%index == 0) then
                        call restrict(windows(window%index)%start, 0_int64, b)
                    end if

                    if (windows(window%index)%pointm%index == 0) then
                        call restrict(windows(window%index)%pointm, buffers(bi)%pt, b)
                    end if
                else if (p%start%index /= 0) then
                    ! Leaf window has no live buffer, get one.
                    b = other_buffer_safely(current_buffer())
                    windows(window%index)%buffer = b
                    call window_discard_buffer_from_window(b, window, .false.)
                    call restrict(windows(window%index)%start, 0_int64, b)
                    call restrict(windows(window%index)%pointm, 0_int64, b)

                    ! Try to delete a window that was dedicated to its
                    ! closed buffer later.
                    if (p%dedicated /= DEDICATED_NONE) then
                        ndead = ndead + 1
                        dead_windows(ndead) = window
                    end if

                    windows(window%index)%dedicated = DEDICATED_NONE
                end if
            end associate
        end do

        selected_frame%root_window = config%root_window

        ! Resize the frame's windows if necessary: adjust_frame_size with
        ! the frame's size, here when the configuration was saved with
        ! another top margin (tab bar lines) than the frame has now.
        if (windows(config%root_window%index)%top_line /= frame_top_margin(selected_frame) .and. &
            frame_lines(selected_frame) > 0) then
            call resize_frame_windows(selected_frame, (frame_lines(selected_frame) - &
                                                       frame_top_margin(selected_frame))*selected_frame%line_height, .false.)
        end if

        ! Arrange *not* to restore point in the buffer that was current
        ! when the window configuration was saved.
        if (window_current_shows(config%current_window, new_current_buffer)) then
            call restrict(windows(config%current_window%index)%pointm, old_point, new_current_buffer)
        end if

        ! Select the window without swapping out point in the old
        ! selected window, and without recording the buffer.
        selected_frame%selected_window = config%current_window
        call set_buffer(windows(config%current_window%index)%buffer, status)
        call goto_char(window_point(windows(config%current_window%index)))
        call resolve_buffer(windows(config%current_window%index)%buffer, bi, status)
        if (status%ok()) buffers(bi)%last_selected_window = config%current_window

        ! Free the windows that were not reused.
        do k = 1, n
            if (windows(old(k)%index)%dead) then
                if (windows(old(k)%index)%config_refs == 0) then
                    call destroy_dead_window(old(k))
                end if
            end if
        end do

        ! Scan dead buffer windows.
        do k = 1, ndead
            if (is_window_live(dead_windows(k)) .and. .not. same_window(dead_windows(k), selected_frame%root_window)) then
                call delete_window(dead_windows(k), status)
            end if
        end do

        ! Record the selected window's buffer.
        if (is_window_live(config%current_window)) call select_window(config%current_window, status)

        if (is_buffer_live(new_current_buffer)) then
            call set_buffer(new_current_buffer, status)

            ! If the new current buffer doesn't appear in the selected
            ! window, go to its old point (Bug#12208).
            if (.not. same_handle(windows(selected_frame%selected_window%index)%buffer, new_current_buffer)) then
                call goto_char(old_point)
            end if
        end if

        minibuffer_scroll_window = config%minibuf_scroll_window
    end subroutine set_window_configuration

    !> Free dead window `h' for good once no window configuration refers
    !> to it.  Not a port.
    subroutine destroy_dead_window(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status

        if (.not. wtable%is_live(h)) return
        if (.not. windows(h%index)%dead .or. windows(h%index)%config_refs > 0) return
        call destroy_window(windows(h%index))
        windows(h%index) = window_type()
        call wtable%release(h, status)
    end subroutine destroy_dead_window

    !> Whether window `h' (valid) shows buffer `b'.  Not a port.
    logical function window_current_shows(h, b)
        type(handle_type), intent(in) :: h, b

        window_current_shows = .false.
        if (.not. wtable%is_live(h)) return
        if (b%index == 0) return
        window_current_shows = same_handle(windows(h%index)%buffer, b)
    end function window_current_shows

    module subroutine destroy_window_configuration(config)
        type(window_configuration_type), intent(inout) :: config
        type(status_type) :: status
        integer :: k

        if (.not. allocated(config%saved)) return

        do k = 1, size(config%saved)
            associate (p => config%saved(k))
                if (p%start%index /= 0) call destroy_marker(p%start, status)
                if (p%pointm%index /= 0) call destroy_marker(p%pointm, status)

                if (wtable%is_live(p%window)) then
                    windows(p%window%index)%config_refs = max(0, windows(p%window%index)%config_refs - 1)
                    call destroy_dead_window(p%window)
                end if
            end associate
        end do

        deallocate (config%saved)
        config%root_window = null_handle()
        config%current_window = null_handle()
        config%current_buffer = null_handle()
    end subroutine destroy_window_configuration

    module subroutine copy_window_configuration(config, copy)
        type(window_configuration_type), intent(in) :: config
        type(window_configuration_type), intent(out) :: copy
        type(status_type) :: status
        integer :: k

        copy%root_window = config%root_window
        copy%current_window = config%current_window
        copy%current_buffer = config%current_buffer
        copy%minibuf_scroll_window = config%minibuf_scroll_window
        copy%frame_cols = config%frame_cols
        copy%frame_lines = config%frame_lines
        if (.not. allocated(config%saved)) return
        allocate (copy%saved(size(config%saved)))

        do k = 1, size(config%saved)
            associate (p => config%saved(k), q => copy%saved(k))
                q%window = p%window
                q%buffer = p%buffer
                q%left_col = p%left_col
                q%top_line = p%top_line
                q%total_cols = p%total_cols
                q%total_lines = p%total_lines
                q%hscroll = p%hscroll
                q%dedicated = p%dedicated
                q%divider = p%divider
                q%normal_lines = p%normal_lines
                q%normal_cols = p%normal_cols
                q%parent = p%parent
                q%prev = p%prev
                q%start = null_handle()
                q%pointm = null_handle()
                if (p%start%index /= 0) call copy_saved_marker(p%start, p%buffer, q%start)
                if (p%pointm%index /= 0) call copy_saved_marker(p%pointm, p%buffer, q%pointm)
                if (wtable%is_live(p%window)) windows(p%window%index)%config_refs = windows(p%window%index)%config_refs + 1
            end associate
        end do

    contains

        !> A new marker M in BUFFER where marker FROM points.  Not a port.
        subroutine copy_saved_marker(from, buffer, m)
            type(handle_type), intent(in) :: from, buffer
            type(handle_type), intent(out) :: m
            integer(int64) :: pos

            call make_marker(m)
            call marker_position(from, pos, status)
            if (.not. status%ok()) return
            if (is_buffer_live(buffer)) call set_marker(m, pos, status, buffer)
        end subroutine copy_saved_marker
    end subroutine copy_window_configuration

    module function is_window_configuration_equal(x, y) result(equal)
        type(window_configuration_type), intent(in) :: x, y
        logical :: equal
        integer :: i

        equal = .false.
        if (.not. allocated(x%saved) .or. .not. allocated(y%saved)) return
        ! Frame settings must match.
        if (x%frame_cols /= y%frame_cols .or. x%frame_lines /= y%frame_lines) return
        if (.not. same_handle(x%current_buffer, y%current_buffer)) return
        ! Verify that the two configurations have the same number of windows.
        if (size(x%saved) /= size(y%saved)) return

        do i = 1, size(x%saved)
            associate (sw1 => x%saved(i), sw2 => y%saved(i))
                ! The "current" windows in the two configurations must
                ! correspond to each other.
                if (same_handle(x%current_window, sw1%window) .neqv. same_handle(y%current_window, sw2%window)) return
                ! Windows' buffers must match.
                if (.not. same_handle(sw1%buffer, sw2%buffer)) return
                if (sw1%left_col /= sw2%left_col .or. sw1%top_line /= sw2%top_line .or. &
                    sw1%total_cols /= sw2%total_cols .or. sw1%total_lines /= sw2%total_lines) return
                ! The next two disjuncts check the window structure for
                ! equality.
                if (sw1%parent /= sw2%parent .or. sw1%prev /= sw2%prev) return
            end associate
        end do

        equal = .true.
    end function is_window_configuration_equal

    ! ------------------------------------------------------------------
    ! Window states
    ! ------------------------------------------------------------------

    module subroutine get_window_state(state)
        type(window_state_type), intent(out) :: state
        integer :: n

        call ensure_frame_windows_here()
        allocate (state%windows(count_windows(selected_frame%root_window)))
        n = get_window_state_1(selected_frame%root_window, state%windows, 1, 0)
    end subroutine get_window_state

    !> Record `h' and its next siblings, with their children, in WINDOWS
    !> from index I on (depth first); PARENT is the index of their parent.
    !> Return the next free index.  Port of window.el:window--state-get-1.
    recursive integer function get_window_state_1(h, states, i, parent) result(next)
        type(handle_type), intent(in) :: h
        type(window_state_window_type), intent(inout) :: states(:)
        integer, intent(in) :: i, parent
        type(handle_type) :: w, root
        type(status_type) :: status
        integer(int64) :: pos, bi
        integer :: prev, k

        next = i
        prev = 0
        w = h
        root = selected_frame%root_window

        do while (is_window_valid(w))
            associate (p => states(next), x => windows(w%index))
                p%parent = parent
                p%prev = prev
                p%live = .not. is_window_valid(x%child)
                p%horizontal = x%horizontal
                p%divider = x%divider
                p%left_col = x%left_col - windows(root%index)%left_col
                p%top_line = x%top_line - windows(root%index)%top_line
                p%total_cols = x%total_cols
                p%total_lines = x%total_lines
                p%normal_lines = x%normal_lines
                p%normal_cols = x%normal_cols

                if (p%live .and. is_buffer_live(x%buffer)) then
                    p%buffer = x%buffer
                    p%selected = same_window(w, selected_frame%selected_window)
                    p%hscroll = x%hscroll
                    p%dedicated = x%dedicated

                    ! The selected window's point is its buffer's.
                    if (p%selected) then
                        call resolve_buffer(p%buffer, bi, status)
                        pos = buffers(bi)%pt
                    else
                        pos = window_point(x)
                    end if

                    call make_marker(p%point)
                    call set_marker(p%point, pos, status, p%buffer)
                    call make_marker(p%start)
                    call set_marker(p%start, window_start(x), status, p%buffer)

                    if (allocated(x%prev_buffers)) then
                        allocate (p%prev_buffers(size(x%prev_buffers)))

                        do k = 1, size(x%prev_buffers)
                            p%prev_buffers(k)%buffer = x%prev_buffers(k)%buffer
                            call copy_saved_marker(x%prev_buffers(k)%start, x%prev_buffers(k)%buffer, &
                                                   p%prev_buffers(k)%start)
                            call copy_saved_marker(x%prev_buffers(k)%point, x%prev_buffers(k)%buffer, &
                                                   p%prev_buffers(k)%point)
                        end do
                    end if

                    if (allocated(x%next_buffers)) p%next_buffers = x%next_buffers
                end if
            end associate

            prev = next
            next = next + 1

            if (is_window_valid(windows(w%index)%child)) then
                next = get_window_state_1(windows(w%index)%child, states, next, prev)
            end if

            w = windows(w%index)%next
        end do

    contains

        !> A new marker M in BUFFER where marker FROM points (null when
        !> FROM is).  Not a port.
        subroutine copy_saved_marker(from, buffer, m)
            type(handle_type), intent(in) :: from, buffer
            type(handle_type), intent(out) :: m
            integer(int64) :: pos

            m = null_handle()
            if (from%index == 0) return
            call make_marker(m)
            call marker_position(from, pos, status)
            if (.not. status%ok()) return
            if (is_buffer_live(buffer)) call set_marker(m, pos, status, buffer)
        end subroutine copy_saved_marker
    end function get_window_state_1

    module subroutine set_window_state(state, status)
        type(window_state_type), intent(in) :: state
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: made(:), leaves(:)
        type(handle_type) :: root, selected, b
        integer :: k, top, left

        status = status_type()
        call ensure_frame_windows_here()
        if (.not. allocated(state%windows)) return
        if (size(state%windows) == 0) return
        root = selected_frame%root_window

        if (state%windows(1)%total_cols /= windows(root%index)%total_cols .or. &
            state%windows(1)%total_lines /= windows(root%index)%total_lines) then
            call set_error(status, FM_GENERIC_ERROR, "Window state of another size")
            return
        end if

        top = windows(root%index)%top_line
        left = windows(root%index)%left_col
        ! Delete the frame's windows.
        call window_leaves(root, leaves)

        do k = 1, size(leaves)
            call unshow_buffer(leaves(k))
        end do

        call delete_window_tree(root)
        ! Make the new ones.
        allocate (made(size(state%windows)))
        selected = null_handle()

        do k = 1, size(state%windows)
            call make_window(made(k))
        end do

        do k = 1, size(state%windows)
            associate (p => state%windows(k), w => windows(made(k)%index))
                w%left_col = left + p%left_col
                w%top_line = top + p%top_line
                w%total_cols = p%total_cols
                w%total_lines = p%total_lines
                w%normal_lines = p%normal_lines
                w%normal_cols = p%normal_cols
                w%horizontal = p%horizontal
                w%divider = p%divider
                if (p%parent /= 0) w%parent = made(p%parent)

                if (p%prev /= 0) then
                    w%prev = made(p%prev)
                    windows(made(p%prev)%index)%next = made(k)
                else if (p%parent /= 0) then
                    windows(made(p%parent)%index)%child = made(k)
                end if
            end associate

            if (.not. state%windows(k)%live) cycle
            b = state%windows(k)%buffer
            if (.not. is_buffer_live(b)) b = other_buffer_safely(current_buffer())
            call set_window_buffer_internal(made(k), b, .false.)

            if (same_handle(b, state%windows(k)%buffer)) then
                call restrict(windows(made(k)%index)%start, marker_pos(state%windows(k)%start), b)
                call restrict(windows(made(k)%index)%pointm, marker_pos(state%windows(k)%point), b)
                windows(made(k)%index)%hscroll = state%windows(k)%hscroll
                windows(made(k)%index)%dedicated = state%windows(k)%dedicated

                if (allocated(state%windows(k)%next_buffers)) then
                    windows(made(k)%index)%next_buffers = state%windows(k)%next_buffers
                end if

                call put_prev_buffers(made(k), state%windows(k))
                if (state%windows(k)%selected) selected = made(k)
            end if
        end do

        selected_frame%root_window = made(1)
        if (.not. is_window_live(selected)) call first_live(selected)
        call select_window(selected, status)

    contains

        !> The first live window made, in SELECTED.  Not a port.
        subroutine first_live(window)
            type(handle_type), intent(out) :: window
            integer :: j

            window = made(1)

            do j = 1, size(made)
                if (is_window_live(made(j))) then
                    window = made(j)
                    return
                end if
            end do
        end subroutine first_live
    end subroutine set_window_state

    !> Give window H the previous buffers of state window P (those still
    !> live), with markers of their own.  Port of
    !> window.el:window--state-put-2 (its prev-buffers part).
    subroutine put_prev_buffers(h, p)
        type(handle_type), intent(in) :: h
        type(window_state_window_type), intent(in) :: p
        type(prev_buffer_type), allocatable :: entries(:)
        type(handle_type) :: m
        type(status_type) :: status
        integer :: k, n

        if (.not. allocated(p%prev_buffers)) return
        allocate (entries(size(p%prev_buffers)))
        n = 0

        do k = 1, size(p%prev_buffers)
            if (.not. is_buffer_live(p%prev_buffers(k)%buffer)) cycle
            n = n + 1
            entries(n)%buffer = p%prev_buffers(k)%buffer
            call make_marker(m)
            call set_marker(m, marker_pos(p%prev_buffers(k)%start), status, entries(n)%buffer)
            entries(n)%start = m
            call make_marker(m)
            call set_marker(m, marker_pos(p%prev_buffers(k)%point), status, entries(n)%buffer)
            entries(n)%point = m
        end do

        windows(h%index)%prev_buffers = entries(1:n)
    end subroutine put_prev_buffers

    module subroutine destroy_window_state(state)
        type(window_state_type), intent(inout) :: state
        type(status_type) :: status
        integer :: k, j

        if (.not. allocated(state%windows)) return

        do k = 1, size(state%windows)
            associate (p => state%windows(k))
                if (p%start%index /= 0) call destroy_marker(p%start, status)
                if (p%point%index /= 0) call destroy_marker(p%point, status)

                if (allocated(p%prev_buffers)) then
                    do j = 1, size(p%prev_buffers)
                        if (p%prev_buffers(j)%start%index /= 0) call destroy_marker(p%prev_buffers(j)%start, status)
                        if (p%prev_buffers(j)%point%index /= 0) call destroy_marker(p%prev_buffers(j)%point, status)
                    end do
                end if
            end associate
        end do

        deallocate (state%windows)
    end subroutine destroy_window_state
end submodule window_configuration_mod
