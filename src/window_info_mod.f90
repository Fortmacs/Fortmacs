! window_info_mod.f90 -- what windows are: the window tree, sizes and the
! cyclic order of windows.
! Ported from GNU Emacs 31.1 src/window.c (in part: the accessors of
! struct window, next-window, window-list, coordinates-in-window-p and
! window-at).
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

!> Part of window_mod: the window.c functions that tell about windows.  A
!> window argument that is absent means the selected window; a result
!> of none is the null handle.  There is one frame, so the ALL-FRAMES
!> arguments of upstream make no difference and are left out, and sizes
!> are in the canonical lines and columns of a terminal frame.  These
!> need the frame (frame_mod), so they live in a submodule.
submodule(window_mod) window_info_mod
    use error_mod, only: set_error, FM_WRONG_TYPE_ARGUMENT
    use frame_mod, only: selected_frame, selected_window, ensure_frame_windows
    use keyboard_mod, only: minib_level => minibuf_level
    use minibuf_mod, only: minibuffer_depth_of
    implicit none(type, external)

contains

    !> The window WINDOW names, or the selected window if absent, as an
    !> index into `windows'; an error unless it is live.  Port of
    !> window.c:decode_live_window.
    subroutine decode_live_window(window, i, status)
        type(handle_type), intent(in), optional :: window
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status
        type(handle_type) :: h

        h = decoded(window)
        i = 0

        if (.not. is_window_live(h)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected live type(window_type)")
            return
        end if

        status = status_type()
        i = h%index
    end subroutine decode_live_window

    !> Like `decode_live_window', for a valid window (live or internal).
    !> Port of window.c:decode_valid_window.
    subroutine decode_valid_window(window, i, status)
        type(handle_type), intent(in), optional :: window
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status
        type(handle_type) :: h

        h = decoded(window)
        i = 0

        if (.not. is_window_valid(h)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected valid type(window_type)")
            return
        end if

        status = status_type()
        i = h%index
    end subroutine decode_valid_window

    !> WINDOW, or the selected window if absent.  Not a port.
    function decoded(window) result(h)
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: h

        if (present(window)) then
            h = window
        else
            h = selected_window()
        end if
    end function decoded

    module function is_window(object) result(p)
        type(handle_type), intent(in) :: object
        logical :: p

        p = wtable%issued(object)
    end function is_window

    module function window_buffer(window, status) result(b)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: b, h

        ! window.c:decode_any_window: any window, deleted ones too.
        h = decoded(window)
        b = null_handle()

        if (.not. wtable%issued(h)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(window_type)")
            return
        end if

        status = status_type()
        if (is_window_live(h)) b = windows(h%index)%buffer
    end function window_buffer

    module function window_parent(window, status) result(p)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: p
        integer(int64) :: i

        p = null_handle()
        call decode_valid_window(window, i, status)
        if (status%ok()) p = windows(i)%parent
    end function window_parent

    module function window_top_child(window, status) result(c)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: c
        integer(int64) :: i

        c = null_handle()
        call decode_valid_window(window, i, status)
        if (.not. status%ok()) return
        if (.not. windows(i)%horizontal) c = windows(i)%child
    end function window_top_child

    module function window_left_child(window, status) result(c)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: c
        integer(int64) :: i

        c = null_handle()
        call decode_valid_window(window, i, status)
        if (.not. status%ok()) return
        if (windows(i)%horizontal) c = windows(i)%child
    end function window_left_child

    module function window_next_sibling(window, status) result(s)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: s
        integer(int64) :: i

        s = null_handle()
        call decode_valid_window(window, i, status)
        if (status%ok()) s = windows(i)%next
    end function window_next_sibling

    module function window_prev_sibling(window, status) result(s)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: s
        integer(int64) :: i

        s = null_handle()
        call decode_valid_window(window, i, status)
        if (status%ok()) s = windows(i)%prev
    end function window_prev_sibling

    module function frame_root_window(window, status) result(r)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: r

        r = null_handle()
        status = status_type()

        if (present(window)) then
            if (.not. is_window_valid(window)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected live type(frame_type)")
                return
            end if
        end if

        call ensure_frame_windows()
        r = selected_frame%root_window
    end function frame_root_window

    module function frame_selected_window(window, status) result(s)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: s

        s = null_handle()
        status = status_type()

        if (present(window)) then
            if (.not. is_window_valid(window)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected live type(frame_type)")
                return
            end if
        end if

        s = selected_window()
    end function frame_selected_window

    module function minibuffer_window() result(m)
        type(handle_type) :: m

        call ensure_frame_windows()
        m = selected_frame%minibuffer_window
    end function minibuffer_window

    module function is_window_minibuffer(window, status) result(p)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        logical :: p
        integer(int64) :: i

        p = .false.
        call decode_valid_window(window, i, status)
        if (status%ok()) p = windows(i)%mini
    end function is_window_minibuffer

    module function minibuffer_selected_window() result(s)
        type(handle_type) :: s

        s = null_handle()
        call ensure_frame_windows()

        if (minib_level > 0 .and. selected_frame%minibuffer_selected) then
            if (is_window_live(selected_frame%selected_window)) s = selected_frame%selected_window
        end if
    end function minibuffer_selected_window

    module function window_total_height(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_valid_window(window, i, status)
        if (.not. status%ok()) return
        n = windows(i)%total_lines
    end function window_total_height

    module function window_total_width(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_valid_window(window, i, status)
        if (.not. status%ok()) return
        n = windows(i)%total_cols
    end function window_total_width

    module function window_left_column(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_valid_window(window, i, status)
        if (status%ok()) n = windows(i)%left_col
    end function window_left_column

    module function window_top_line(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_valid_window(window, i, status)
        if (status%ok()) n = windows(i)%top_line
    end function window_top_line

    module function window_body_height(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
        if (status%ok()) n = window_body_lines(windows(i))
    end function window_body_height

    module function window_body_width(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
        if (status%ok()) n = window_text_cols(windows(i))
    end function window_body_width

    module function window_wants_mode_line(w) result(wants)
        type(window_type), intent(in) :: w
        logical :: wants

        wants = w%child%index == 0 .and. .not. w%mini .and. w%total_lines > 1
    end function window_wants_mode_line

    module function window_mode_line_height(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        if (window_wants_mode_line(windows(i))) n = 1
    end function window_mode_line_height

    module function window_header_line_height(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        if (window_wants_header_line(windows(i))) n = 1
    end function window_header_line_height

    module function window_tab_line_height(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
    end function window_tab_line_height

    module function window_hscroll(window, status) result(n)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: n
        integer(int64) :: i

        n = 0
        call decode_live_window(window, i, status)
        if (status%ok()) n = windows(i)%hscroll
    end function window_hscroll

    module function window_dedication(window, status) result(d)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer :: d
        integer(int64) :: i

        d = DEDICATED_NONE
        call decode_live_window(window, i, status)
        if (status%ok()) d = windows(i)%dedicated
    end function window_dedication

    module subroutine set_window_dedication(window, flag, status)
        type(handle_type), intent(in), optional :: window
        integer, intent(in) :: flag
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call decode_live_window(window, i, status)
        if (status%ok()) windows(i)%dedicated = flag
    end subroutine set_window_dedication

    module function window_use_time(window, status) result(t)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer(int64) :: t
        integer(int64) :: i

        t = 0
        call decode_live_window(window, i, status)
        if (status%ok()) t = windows(i)%use_time
    end function window_use_time

    module function window_bump_use_time(window, status) result(t)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        integer(int64) :: t
        integer(int64) :: i, s
        type(handle_type) :: sw

        t = -1
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        sw = selected_window()
        s = sw%index

        if (i /= s .and. windows(s)%use_time == window_select_count) then
            windows(i)%use_time = window_select_count
            window_select_count = window_select_count + 1
            windows(s)%use_time = window_select_count
            t = windows(i)%use_time
        end if
    end function window_bump_use_time

    module function window_prev_buffers(window, status) result(list)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(prev_buffer_type), allocatable :: list(:)
        integer(int64) :: i

        allocate (list(0))
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        if (allocated(windows(i)%prev_buffers)) list = windows(i)%prev_buffers
    end function window_prev_buffers

    module function window_next_buffers(window, status) result(list)
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        integer(int64) :: i

        allocate (list(0))
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        if (allocated(windows(i)%next_buffers)) list = windows(i)%next_buffers
    end function window_next_buffers

    !> The live windows of the frame in cyclic order, the minibuffer
    !> window last.  Port of window.c:window_list (with
    !> window.c:add_window_to_list, window.c:foreach_window and
    !> window.c:foreach_window_1: one frame).
    subroutine all_windows(list)
        type(handle_type), allocatable, intent(out) :: list(:)

        call ensure_frame_windows()
        call window_leaves(selected_frame%root_window, list)
        list = [list, selected_frame%minibuffer_window]
    end subroutine all_windows

    !> The live window WINDOW (selected if absent) and whether the
    !> minibuffer window counts for MINIBUF (MINIBUF_ACTIVE: if the
    !> minibuffer is active).  Port of window.c:decode_next_window_args
    !> (one frame).
    subroutine decode_next_window_args(window, minibuf, w, mini, status)
        type(handle_type), intent(in), optional :: window
        integer, intent(in), optional :: minibuf
        type(handle_type), intent(out) :: w
        logical, intent(out) :: mini
        type(status_type), intent(out) :: status
        integer(int64) :: i
        integer :: m

        w = null_handle()
        mini = .false.
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        w = decoded(window)
        m = MINIBUF_ACTIVE
        if (present(minibuf)) m = minibuf

        select case (m)
        case (MINIBUF_ACTIVE)
            mini = minibuffer_depth_of(windows(selected_frame%minibuffer_window%index)%buffer) > 0
        case (MINIBUF_YES)
            mini = .true.
        case default
            mini = .false.
        end select
    end subroutine decode_next_window_args

    !> Whether WINDOW is a window to consider: live, and the minibuffer
    !> window only if MINI.  Port of window.c:candidate_window_p (one
    !> frame).
    logical function is_candidate_window(window, mini) result(p)
        type(handle_type), intent(in) :: window
        logical, intent(in) :: mini

        p = is_window_live(window)
        if (p .and. windows(window%index)%mini) p = mini
    end function is_candidate_window

    !> The window after (NEXT_P) or before WINDOW in the cyclic order of
    !> the windows MINIBUF admits.  Port of window.c:next_window.
    function next_window_1(window, minibuf, next_p, status) result(r)
        type(handle_type), intent(in), optional :: window
        integer, intent(in), optional :: minibuf
        logical, intent(in) :: next_p
        type(status_type), intent(out) :: status
        type(handle_type) :: r, w, candidate
        type(handle_type), allocatable :: list(:)
        logical :: mini
        integer :: k, at, n

        r = null_handle()
        call decode_next_window_args(window, minibuf, w, mini, status)
        if (.not. status%ok()) return
        r = w
        call all_windows(list)
        n = size(list)
        at = 0

        do k = 1, n
            if (same_window(list(k), w)) at = k
        end do

        if (next_p) then
            ! Forward from WINDOW to the end, then from the start up to it.
            if (at > 0) then
                do k = at + 1, n
                    if (is_candidate_window(list(k), mini)) then
                        r = list(k)
                        return
                    end if
                end do
            end if

            do k = 1, n
                if (same_window(list(k), w)) exit

                if (is_candidate_window(list(k), mini)) then
                    r = list(k)
                    return
                end if
            end do
        else
            ! The last candidate before WINDOW, else the last after it.
            candidate = null_handle()

            do k = 1, n
                if (same_window(list(k), w)) then
                    if (candidate%index /= 0) exit
                else if (is_candidate_window(list(k), mini)) then
                    candidate = list(k)
                end if
            end do

            if (candidate%index /= 0) r = candidate
        end if
    end function next_window_1

    module function next_window(window, minibuf, status) result(r)
        type(handle_type), intent(in), optional :: window
        integer, intent(in), optional :: minibuf
        type(status_type), intent(out) :: status
        type(handle_type) :: r

        r = next_window_1(window, minibuf, .true., status)
    end function next_window

    module function previous_window(window, minibuf, status) result(r)
        type(handle_type), intent(in), optional :: window
        integer, intent(in), optional :: minibuf
        type(status_type), intent(out) :: status
        type(handle_type) :: r

        r = next_window_1(window, minibuf, .false., status)
    end function previous_window

    module function window_list_1(window, minibuf, status) result(list)
        type(handle_type), intent(in), optional :: window
        integer, intent(in), optional :: minibuf
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:), all(:)
        type(handle_type) :: w
        logical :: mini
        integer :: k, n, at

        allocate (list(0))
        call decode_next_window_args(window, minibuf, w, mini, status)
        if (.not. status%ok()) return
        call all_windows(all)
        deallocate (list)
        allocate (list(size(all)))
        n = 0
        at = 0

        do k = 1, size(all)
            if (is_candidate_window(all(k), mini)) then
                n = n + 1
                list(n) = all(k)
                if (same_window(all(k), w)) at = n
            end if
        end do

        ! Rotate the list to start with WINDOW.
        if (at > 1) then
            list(1:n) = [list(at:n), list(1:at - 1)]
        end if

        list = list(1:n)
    end function window_list_1

    module function window_list(minibuf, window, status) result(list)
        integer, intent(in), optional :: minibuf
        type(handle_type), intent(in), optional :: window
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)

        if (present(window)) then
            if (.not. wtable%issued(window)) then
                allocate (list(0))
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(window_type)")
                return
            end if
        end if

        list = window_list_1(window, minibuf, status)
    end function window_list

    !> The part of window W at column X and line Y of the frame (both
    !> 0-based), on a terminal frame.  Port of
    !> window.c:coordinates_in_window (no dividers, scroll bars, fringes,
    !> margins or tab line on a terminal).
    integer function coordinates_in_window(w, x, y) result(part)
        type(window_type), intent(in) :: w
        integer, intent(in) :: x, y
        integer :: left_x, right_x, top_y, bottom_y

        left_x = w%left_col
        right_x = w%left_col + w%total_cols
        top_y = w%top_line
        bottom_y = w%top_line + w%total_lines
        part = ON_NOTHING
        if (y < top_y .or. y >= bottom_y .or. x < left_x .or. x >= right_x) return

        if (window_wants_mode_line(w) .and. y >= bottom_y - 1) then
            part = ON_MODE_LINE
            return
        end if

        if (window_wants_header_line(w) .and. y < top_y + 1) then
            part = ON_HEADER_LINE
            return
        end if

        ! The vertical line in the last column of a window that has a
        ! window on its right.
        if (w%divider .and. x > right_x - 2) then
            part = ON_VERTICAL_BORDER
            return
        end if

        part = ON_TEXT
    end function coordinates_in_window

    module function window_part_at(x, y, window, rel_x, rel_y, status) result(part)
        integer, intent(in) :: x, y
        type(handle_type), intent(in), optional :: window
        integer, intent(out) :: rel_x, rel_y
        type(status_type), intent(out) :: status
        integer :: part
        integer(int64) :: i

        part = ON_NOTHING
        rel_x = 0
        rel_y = 0
        call decode_live_window(window, i, status)
        if (.not. status%ok()) return
        part = coordinates_in_window(windows(i), x, y)

        if (part == ON_TEXT) then
            rel_x = x - windows(i)%left_col
            rel_y = y - windows(i)%top_line
        end if
    end function window_part_at

    !> The window at column X and line Y of the frame, and in PART which
    !> part of it; the null handle for none.  Port of
    !> window.c:window_from_coordinates (with window.c:check_window_containing;
    !> no menu bar, tab bar or tool bar windows).
    function window_from_coordinates(x, y, part) result(window)
        integer, intent(in) :: x, y
        integer, intent(out) :: part
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: k

        window = null_handle()
        part = ON_NOTHING
        call all_windows(list)

        do k = 1, size(list)
            part = coordinates_in_window(windows(list(k)%index), x, y)

            if (part /= ON_NOTHING) then
                window = list(k)
                return
            end if
        end do
    end function window_from_coordinates

    module function window_at(x, y) result(window)
        integer, intent(in) :: x, y
        type(handle_type) :: window
        integer :: part

        window = null_handle()
        call ensure_frame_windows()
        if (x < 0 .or. y < 0) return
        window = window_from_coordinates(x, y, part)
    end function window_at

    module function force_window_update(window, buffer) result(done)
        type(handle_type), intent(in), optional :: window, buffer
        logical :: done
        type(handle_type), allocatable :: list(:)
        integer :: k

        ! Redisplay recomputes every window, so there is nothing to mark:
        ! only the value is left to compute.
        done = .true.
        if (present(window)) done = is_window_live(window)
        if (.not. present(buffer)) return
        done = .false.
        call all_windows(list)

        do k = 1, size(list)
            if (same_window(windows(list(k)%index)%buffer, buffer) .and. .not. windows(list(k)%index)%mini) done = .true.
        end do
    end function force_window_update
end submodule window_info_mod
