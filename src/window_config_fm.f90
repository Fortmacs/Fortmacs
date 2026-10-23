! window_config_fm.f90 -- window configurations.
! Ported from GNU Emacs 31.1 src/window.c (save_window_save,
! Fcurrent_window_configuration, Fset_window_configuration,
! delete_all_child_windows).
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
!> ported), `window_restore_killed_buffer_windows' other than nil, the
!> frame's focus and `minibuffer_scroll_window'.
submodule(window_fm) window_config_fm
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_live_p, resolve_buffer, buffer_list, &
                         get_buffer_create
    use marker_fm, only: make_marker, free_marker, set_marker, set_marker_restricted, marker_position
    use editfns_fm, only: goto_char
    use frame_fm, only: selected_frame
    use minibuf_fm, only: minibufferp, minibuf_level
    implicit none(type, external)

contains

    !> The number of windows in the tree under `h' and its next siblings.
    !> Port of window.c:count_windows.
    recursive integer function count_windows(h) result(n)
        type(handle_type), intent(in) :: h
        type(handle_type) :: w
        n = 0
        w = h
        do while (window_valid_p(w))
            n = n + 1
            if (window_valid_p(windows(w%index)%child)) n = n + count_windows(windows(w%index)%child)
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
        do while (window_valid_p(w))
            associate (p => saved(next), x => windows(w%index))
                p%window = w
                p%buffer = null_handle()
                if (.not. window_valid_p(x%child)) p%buffer = x%buffer
                p%left_col = x%left_col
                p%top_line = x%top_line
                p%total_cols = x%total_cols
                p%total_lines = x%total_lines
                p%divider = x%divider
                p%hscroll = x%hscroll
                p%dedicated = x%dedicated
                p%parent = parent
                p%prev = prev
                x%config_refs = x%config_refs + 1
                if (buffer_live_p(p%buffer)) then
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
            if (window_valid_p(windows(w%index)%child)) then
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
    end subroutine current_window_configuration

    !> The frame has its windows.
    subroutine ensure_frame_windows_here()
        if (.not. window_valid_p(selected_frame%root_window)) then
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
        if (window_valid_p(windows(h%index)%next)) call delete_all_child_windows(windows(h%index)%next, old, n)
        n = n + 1
        old(n) = h
        associate (w => windows(h%index))
            if (window_valid_p(w%child)) then
                call delete_all_child_windows(w%child, old, n)
                w%child = null_handle()
            else if (buffer_live_p(w%buffer)) then
                call unshow_buffer(h)
                if (w%pointm%index /= 0) call free_marker(w%pointm, status)
                if (w%start%index /= 0) call free_marker(w%start, status)
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
    !> `buffer' at `pos'.
    subroutine restrict(m, pos, buffer)
        type(handle_type), intent(inout) :: m
        integer(int64), intent(in) :: pos
        type(handle_type), intent(in) :: buffer
        type(status_type) :: status
        if (m%index == 0) call make_marker(m)
        call set_marker_restricted(m, pos, status, buffer)
    end subroutine restrict

    !> The position of marker `m' (1 if it points nowhere).
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
            if (.not. buffer_live_p(all(k))) cycle
            if (same_handle(all(k), buffer)) cycle
            if (len(buffers(all(k)%index)%name) > 0) then
                if (buffers(all(k)%index)%name(1:1) == " ") cycle
            end if
            b = all(k)
            return
        end do
        call get_buffer_create("*scratch*", b, status)
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
        if (.not. buffer_live_p(new_current_buffer)) then
            new_current_buffer = null_handle()
        else
            ! Be careful which point of the current buffer is copied into
            ! old_point: the point of the window to be selected.
            call resolve_buffer(new_current_buffer, bi, status)
            ! The point of the window to be selected, if it shows the
            ! buffer and is not the selected window; else the buffer's.
            old_point = buffers(bi)%pt
            if (window_cur_shows(config%current_window, new_current_buffer) .and. &
                .not. same_window(sw, config%current_window)) then
                if (.not. same_handle(new_current_buffer, current_buffer())) then
                    old_point = window_point(windows(config%current_window%index))
                else if (window_cur_shows(sw, new_current_buffer)) then
                    old_point = window_point(windows(config%current_window%index))
                end if
            end if
        end if
        ! If a window we restore gets another buffer, record the window's
        ! old buffer.
        do k = 1, size(config%saved)
            associate (p => config%saved(k))
                if (.not. window_live_p(p%window)) cycle
                if (.not. buffer_live_p(windows(p%window%index)%buffer)) cycle
                if (same_handle(windows(p%window%index)%buffer, p%buffer)) cycle
                if (.not. buffer_live_p(p%buffer)) cycle
                if (minibufferp(p%buffer)) cycle
                call record_window_buffer(p%window)
            end associate
        end do
        ! "Swap out" point from the selected window's buffer into the
        ! window itself.
        if (window_live_p(sw)) then
            if (buffer_live_p(windows(sw%index)%buffer)) then
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
        if (window_valid_p(selected_frame%root_window)) then
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
                        if (window_valid_p(w%parent)) then
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
                    w%hscroll = p%hscroll
                    w%dedicated = p%dedicated
                    w%window_end_valid = .false.
                end associate
                if (buffer_live_p(p%buffer)) then
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
                else if (buffer_live_p(windows(window%index)%buffer)) then
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
                    ! killed buffer later.
                    if (p%dedicated /= DEDICATED_NONE) then
                        ndead = ndead + 1
                        dead_windows(ndead) = window
                    end if
                    windows(window%index)%dedicated = DEDICATED_NONE
                end if
            end associate
        end do
        selected_frame%root_window = config%root_window
        ! Arrange *not* to restore point in the buffer that was current
        ! when the window configuration was saved.
        if (window_cur_shows(config%current_window, new_current_buffer)) then
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
                    call release_dead_window(old(k))
                end if
            end if
        end do
        ! Scan dead buffer windows.
        do k = 1, ndead
            if (window_live_p(dead_windows(k)) .and. .not. same_window(dead_windows(k), selected_frame%root_window)) then
                call delete_window(dead_windows(k), status)
            end if
        end do
        ! Record the selected window's buffer.
        if (window_live_p(config%current_window)) call select_window(config%current_window, status)
        if (buffer_live_p(new_current_buffer)) then
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
    !> to it.
    subroutine release_dead_window(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status
        if (.not. wtable%is_live(h)) return
        if (.not. windows(h%index)%dead .or. windows(h%index)%config_refs > 0) return
        call free_window(windows(h%index))
        windows(h%index) = window_type()
        call wtable%release(h, status)
    end subroutine release_dead_window

    !> Whether window `h' (valid) shows buffer `b'.
    logical function window_cur_shows(h, b)
        type(handle_type), intent(in) :: h, b
        window_cur_shows = .false.
        if (.not. wtable%is_live(h)) return
        if (b%index == 0) return
        window_cur_shows = same_handle(windows(h%index)%buffer, b)
    end function window_cur_shows

    module subroutine free_window_configuration(config)
        type(window_configuration_type), intent(inout) :: config
        type(status_type) :: status
        integer :: k
        if (.not. allocated(config%saved)) return
        do k = 1, size(config%saved)
            associate (p => config%saved(k))
                if (p%start%index /= 0) call free_marker(p%start, status)
                if (p%pointm%index /= 0) call free_marker(p%pointm, status)
                if (wtable%is_live(p%window)) then
                    windows(p%window%index)%config_refs = max(0, windows(p%window%index)%config_refs - 1)
                    call release_dead_window(p%window)
                end if
            end associate
        end do
        deallocate (config%saved)
        config%root_window = null_handle()
        config%current_window = null_handle()
        config%current_buffer = null_handle()
    end subroutine free_window_configuration
end submodule window_config_fm
