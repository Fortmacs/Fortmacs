! winner_mod.f90 -- Restore old window configurations.
! Ported from GNU Emacs 31.1 lisp/winner.el.
!
! Copyright (C) 1997-1998, 2001-2026 Free Software Foundation, Inc.
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

!> Winner mode is a global minor mode that records the changes in the
!> window configuration (i.e. how the frames are partitioned into
!> windows) so that the changes can be "undone" using the command
!> `undo_window_change'.  By default this one is bound to the key sequence
!> C-c <left>.  If you change your mind (while undoing), you can press
!> C-c <right> (calling `redo_window_change').
!>
!> One frame: upstream's rings and current configurations per frame
!> (`winner-ring-alist', `winner-currents', `winner-modified-list',
!> `winner-last-frames') are one ring and one configuration here.  The
!> ring (ring.el's ring of Lisp objects) is an array of configurations,
!> the newest first, as `ring_ref' counts them.  The keys of
!> `winner_mode_map' are bound the first time Winner mode runs, as loading
!> winner.el binds them.  Deferred: see the PORTING.md row.
module winner_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use callint_mod, only: command_type, this_command
    use keyboard_mod, only: last_command, post_command_hook
    use commands_mod, only: commands
    use hooks_mod, only: hook_type
    use modes_mod, only: minor_modes
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, resolve_buffer
    use marker_mod, only: set_marker_restricted, make_marker
    use editfns_mod, only: goto_char, mark_position
    use simple_mod, only: is_region_active, set_mark, activate_mark, deactivate_mark
    use search_mod, only: string_match
    use keymap_mod, only: make_sparse_keymap, is_keymap_live, destroy_keymap, keymap_set, set_repeat_map
    use value_mod, only: value_type
    use minibuf_mod, only: minibuf_level, minibuffer_setup_hook
    use xdisp_mod, only: message
    use frame_mod, only: selected_frame
    use window_mod, only: window_configuration_type, current_window_configuration, set_window_configuration, &
                          destroy_window_configuration, copy_window_configuration, window_list, windows, &
                          window_leaves, is_window_live, same_window, same_handle, select_window, other_window, delete_window, &
                          window_point, window_configuration_change_hook, MINIBUF_NO
    implicit none(type, external)
    private

    public :: winner_mode, undo_window_change, redo_window_change, init_winner

    !> True means do not bind default keys in Winner mode.  Port of
    !> winner.el winner-dont-bind-my-keys (read the first time Winner mode
    !> runs; its :set function after that is Deferred with Customize).
    logical, public :: winner_dont_bind_my_keys = .false.

    !> Maximum number of stored window configurations per frame.  Port of
    !> winner.el winner-ring-size.
    integer, public :: winner_ring_size = 200

    !> List of buffer names whose windows `undo_window_change' will not restore.
    !> You may want to include buffer names such as *Help*, *Apropos*,
    !> *Buffer List*, *info* and *Compile-Log*.  Port of winner.el
    !> winner-boring-buffers.
    character(len=256), allocatable, public :: winner_boring_buffers(:)

    !> `undo_window_change' will not restore windows with buffers matching this regexp.
    !> Port of winner.el winner-boring-buffers-regexp (unallocated for not set).
    character(len=:), allocatable, public :: winner_boring_buffers_regexp

    !> Functions to run whenever Winner mode is turned on or off.  Port of
    !> winner.el winner-mode-hook.
    type(hook_type), target, public :: winner_mode_hook = hook_type(name="winner_mode_hook")

    !> Keymap for Winner mode.  Port of winner.el winner-mode-map.
    type(handle_type), public :: winner_mode_map

    !> Keymap to repeat winner key sequences.  Used in `repeat_mode'.
    !> Port of winner.el winner-repeat-map.
    type(handle_type), public :: winner_repeat_map

    !> One window of a configuration's data: its edges (left, top, right
    !> and bottom, as `window_edges' gives them) and its buffer.
    type :: winner_window_data_type
        integer :: edges(4) = 0
        type(handle_type) :: buffer
    end type winner_window_data_type

    !> The essential properties of the windows of the frame (winner.el
    !> winner-win-data), sorted by their edges.
    type :: winner_data_type
        type(winner_window_data_type), allocatable :: windows(:)
    end type winner_data_type

    !> A Winner configuration (winner.el winner-conf): the window
    !> configuration and its data.
    type :: winner_conf_type
        type(window_configuration_type) :: config
        type(winner_data_type) :: data
    contains
        !> Free the configuration.
        procedure :: destroy => destroy_winner_conf
    end type winner_conf_type

    !> What `make_winner_point_alist' recorded of a buffer shown in a
    !> window (an element of winner.el winner-point-alist): its mark (0
    !> for none) and whether it was active, and the point of each window
    !> showing it (the null window for the buffer's own point).
    type :: winner_point_type
        type(handle_type) :: buffer
        integer(int64) :: mark = 0
        logical :: mark_active = .false.
        type(handle_type), allocatable :: windows(:)
        integer(int64), allocatable :: points(:)
    end type winner_point_type

    !> The current configuration of the frame (winner.el winner-currents).
    type(winner_conf_type) :: winner_currents
    logical :: has_currents = .false.

    !> The frame's ring of configurations, the newest first, made with
    !> `winner_ring_capacity' elements (winner.el winner-ring-alist).
    type(winner_conf_type), allocatable :: winner_ring_confs(:)
    integer :: winner_ring_capacity = 0
    logical :: has_ring = .false.

    !> The command `save_winner_old_configurations' last saw (winner.el
    !> winner-last-command), and whether the frame's configuration was
    !> saved since (winner.el winner-last-frames).
    type(command_type), pointer :: winner_last_command => null()
    logical :: winner_last_frame_saved = .false.

    !> Whether the frame's window configuration changed (winner.el
    !> winner-modified-list).
    logical :: winner_modified = .false.

    !> Whether `undo_window_change' has run on the frame (winner.el
    !> winner-undo-frame), how far it went (winner.el winner-undo-counter)
    !> and the window data it has passed (winner.el winner-undone-data).
    logical :: undo_window_change_frame_set = .false.
    integer :: undo_window_change_counter = 0
    type(winner_data_type), allocatable :: winner_undone_data(:)

    !> The points and marks `undo_window_change' restores (winner.el
    !> winner-point-alist).
    type(winner_point_type), allocatable :: winner_point_alist(:)

    !> Whether Winner mode has run (loading winner.el binds the keys).
    logical :: winner_loaded = .false.

contains

    !> Whether A and B are the same command (both none, or the same).
    !> Not a port.
    logical function is_same_command(a, b)
        type(command_type), pointer, intent(in) :: a, b

        if (.not. associated(a)) then
            is_same_command = .not. associated(b)
        else
            is_same_command = associated(a, b)
        end if
    end function is_same_command

    !> Free Winner configuration SELF.  Not a port.
    subroutine destroy_winner_conf(self)
        class(winner_conf_type), intent(inout) :: self

        call destroy_window_configuration(self%config)
        if (allocated(self%data%windows)) deallocate (self%data%windows)
    end subroutine destroy_winner_conf

    !> Make COPY a Winner configuration of its own equal to CONF.
    !> Not a port.
    subroutine copy_winner_conf(conf, copy)
        type(winner_conf_type), intent(in) :: conf
        type(winner_conf_type), intent(out) :: copy

        call copy_window_configuration(conf%config, copy%config)
        copy%data = conf%data
    end subroutine copy_winner_conf

    !> Move Winner configuration FROM into TO (FROM is left empty).
    !> Not a port.
    subroutine move_winner_conf(from, to)
        type(winner_conf_type), intent(inout) :: from
        type(winner_conf_type), intent(out) :: to

        call move_alloc(from%config%saved, to%config%saved)
        to%config%root_window = from%config%root_window
        to%config%current_window = from%config%current_window
        to%config%current_buffer = from%config%current_buffer
        to%config%minibuf_scroll_window = from%config%minibuf_scroll_window
        to%config%frame_cols = from%config%frame_cols
        to%config%frame_lines = from%config%frame_lines
        call move_alloc(from%data%windows, to%data%windows)
    end subroutine move_winner_conf

    ! ------------------------------------------------------------------
    ! Saving old configurations (internal variables and subroutines)
    ! ------------------------------------------------------------------

    !> The edges of window H: left, top, right and bottom.  Port of
    !> window.el:window-edges (the total edges, in columns and lines).
    pure function window_edges(h) result(edges)
        type(handle_type), intent(in) :: h
        integer :: edges(4)

        associate (w => windows(h%index))
            edges = [w%left_col, w%top_line, w%left_col + w%total_cols, w%top_line + w%total_lines]
        end associate
    end function window_edges

    !> Whether edges A come before edges B: the first that differ is
    !> smaller.  Not a port.
    pure logical function is_edges_before(a, b)
        integer, intent(in) :: a(4), b(4)
        integer :: k

        is_edges_before = .false.

        do k = 1, 4
            if (a(k) /= b(k)) then
                is_edges_before = a(k) < b(k)
                return
            end if
        end do
    end function is_edges_before

    !> The frame's windows (not the minibuffer window), sorted by their
    !> edges.  Port of winner.el:winner-sorted-window-list.
    function winner_sorted_window_list() result(list)
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: h
        type(status_type) :: status
        integer :: i, j

        list = window_list(MINIBUF_NO, status=status)

        ! An insertion sort, stable as `sort'.
        do i = 2, size(list)
            h = list(i)
            j = i - 1

            do while (j >= 1)
                if (.not. is_edges_before(window_edges(h), window_edges(list(j)))) exit
                list(j + 1) = list(j)
                j = j - 1
            end do

            list(j + 1) = h
        end do
    end function winner_sorted_window_list

    !> Essential properties of the windows in the selected frame: their
    !> edges and buffers.  Port of winner.el:winner-win-data.
    subroutine winner_win_data(data)
        type(winner_data_type), intent(out) :: data
        type(handle_type), allocatable :: list(:)
        integer :: k

        list = winner_sorted_window_list()
        allocate (data%windows(size(list)))

        do k = 1, size(list)
            data%windows(k)%edges = window_edges(list(k))
            data%windows(k)%buffer = windows(list(k)%index)%buffer
        end do
    end subroutine winner_win_data

    !> The current Winner configuration.  Port of winner.el:winner-conf.
    subroutine winner_conf(conf)
        type(winner_conf_type), intent(out) :: conf

        call current_window_configuration(conf%config)
        call winner_win_data(conf%data)
    end subroutine winner_conf

    !> Remember the frame's configuration.  Port of
    !> winner.el:winner-remember.
    subroutine winner_remember()
        if (has_currents) call winner_currents%destroy()
        call winner_conf(winner_currents)
        has_currents = .true.
    end subroutine winner_remember

    !> A copy of the frame's remembered configuration, or else its current
    !> one.  Port of winner.el:winner-configuration.
    subroutine winner_configuration(conf)
        type(winner_conf_type), intent(out) :: conf

        if (has_currents) then
            call copy_winner_conf(winner_currents, conf)
        else
            call winner_conf(conf)
        end if
    end subroutine winner_configuration

    !> Make the frame's ring if it has none, with the frame's
    !> configuration in it.  Port of winner.el:winner-ring.
    subroutine winner_ring()
        type(winner_conf_type) :: conf

        if (has_ring) return
        winner_ring_capacity = winner_ring_size
        allocate (winner_ring_confs(0))
        has_ring = .true.
        call winner_configuration(conf)
        call ring_insert(conf)
    end subroutine winner_ring

    !> Insert CONF at the front of the ring (moving it there); the oldest
    !> element goes when the ring is full.  Port of ring.el:ring-insert.
    subroutine ring_insert(conf)
        type(winner_conf_type), intent(inout) :: conf
        type(winner_conf_type), allocatable :: grown(:)
        integer :: n, k

        n = size(winner_ring_confs)

        if (winner_ring_capacity <= 0) then
            call conf%destroy()
            return
        end if

        if (n == winner_ring_capacity) then
            call winner_ring_confs(n)%destroy()
            n = n - 1
        end if

        allocate (grown(n + 1))
        call move_winner_conf(conf, grown(1))

        do k = 1, n
            call move_winner_conf(winner_ring_confs(k), grown(k + 1))
        end do

        call move_alloc(grown, winner_ring_confs)
    end subroutine ring_insert

    !> Take element INDEX (0 the newest) out of the ring into CONF.
    !> Port of ring.el:ring-remove.
    subroutine ring_remove(index, conf)
        integer, intent(in) :: index
        type(winner_conf_type), intent(out) :: conf
        type(winner_conf_type), allocatable :: shrunk(:)
        integer :: n, k

        n = size(winner_ring_confs)
        call move_winner_conf(winner_ring_confs(index + 1), conf)
        allocate (shrunk(n - 1))

        do k = 1, index
            call move_winner_conf(winner_ring_confs(k), shrunk(k))
        end do

        do k = index + 2, n
            call move_winner_conf(winner_ring_confs(k), shrunk(k - 1))
        end do

        call move_alloc(shrunk, winner_ring_confs)
    end subroutine ring_remove

    !> Return true if two Winner configurations (as produced by
    !> `winner_conf') are equal.  Port of winner.el:winner-equal.
    pure logical function is_winner_equal(a, b)
        type(winner_data_type), intent(in) :: a, b
        integer :: k

        is_winner_equal = .false.
        if (size(a%windows) /= size(b%windows)) return

        do k = 1, size(a%windows)
            if (any(a%windows(k)%edges /= b%windows(k)%edges)) return
            if (a%windows(k)%buffer%index /= b%windows(k)%buffer%index) return
            if (a%windows(k)%buffer%generation /= b%windows(k)%buffer%generation) return
        end do

        is_winner_equal = .true.
    end function is_winner_equal

    !> Save the frame's configuration in its ring, unless it was saved
    !> since the last command or this command is `redo_window_change'.  When the
    !> previous configuration was very similar, keep only the latest.
    !> Port of winner.el:winner-insert-if-new.
    subroutine insert_winner_if_new()
        type(winner_conf_type) :: conf, old
        type(command_type), pointer :: redo

        redo => commands%redo_window_change
        if (winner_last_frame_saved .or. is_same_command(this_command, redo)) return
        call winner_configuration(conf)
        call winner_ring()

        if (size(winner_ring_confs) > 0) then
            if (is_winner_equal(conf%data, winner_ring_confs(1)%data)) then
                call ring_remove(0, old)
                call old%destroy()
            end if
        end if

        call ring_insert(conf)
        winner_last_frame_saved = .true.
    end subroutine insert_winner_if_new

    !> Note that the frame's window configuration changed (on
    !> `window_configuration_change_hook').  Port of
    !> winner.el:winner-change-fun.
    recursive subroutine winner_change_fun(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (minibuf_level == 0) winner_modified = .true.
    end subroutine winner_change_fun

    !> Save the configuration of a changed frame (on `post_command_hook').
    !> Port of winner.el:winner-save-old-configurations.
    recursive subroutine save_winner_old_configurations(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (minibuf_level /= 0) return

        if (.not. is_same_command(this_command, winner_last_command)) then
            winner_last_frame_saved = .false.
            winner_last_command => this_command
        end if

        if (winner_modified) call insert_winner_if_new()
        winner_modified = .false.
        call winner_remember()
    end subroutine save_winner_old_configurations

    !> Save the frame's configuration (on `minibuffer_setup_hook').
    !> Port of winner.el:winner-save-unconditionally.
    recursive subroutine save_winner_unconditionally(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. is_same_command(this_command, winner_last_command)) then
            winner_last_frame_saved = .false.
            winner_last_command => this_command
        end if

        call insert_winner_if_new()
        call winner_remember()
    end subroutine save_winner_unconditionally

    !> Port of winner.el:winner-save-conditionally.
    subroutine save_winner_conditionally()
        type(status_type) :: status

        if (minibuf_level == 0) call save_winner_unconditionally(status)
    end subroutine save_winner_conditionally

    ! ------------------------------------------------------------------
    ! Restoring configurations
    ! ------------------------------------------------------------------

    !> Restore window configuration WINCONF, keeping the selected window
    !> selected if it is still live (else, from the minibuffer, selecting
    !> another).  Port of winner.el:winner-set-conf (the minibuffer window
    !> keeps its buffer and point, as `set_window_configuration' leaves
    !> them, and its height, which nothing changes: resizing the minibuffer
    !> window is Deferred).
    subroutine set_winner_conf(winconf, status)
        type(window_configuration_type), intent(in) :: winconf
        type(status_type), intent(out) :: status
        type(handle_type) :: chosen

        status = status_type()
        chosen = selected_frame%selected_window
        call set_window_configuration(winconf)

        if (is_window_live(chosen)) then
            call select_window(chosen, status)
        else if (selected_frame%minibuffer_selected) then
            call other_window(1_int64, status)
        end if
    end subroutine set_winner_conf

    !> The point of window H: its buffer's point if it is the selected
    !> window.  Port of window.c:Fwindow_point.
    integer(int64) function point_of_window(h) result(pos)
        type(handle_type), intent(in) :: h
        integer(int64) :: bi
        type(status_type) :: status

        if (same_window(h, selected_frame%selected_window)) then
            call resolve_buffer(windows(h%index)%buffer, bi, status)

            if (status%ok()) then
                pos = buffers(bi)%pt
                return
            end if
        end if

        pos = window_point(windows(h%index))
    end function point_of_window

    !> Make POS the point of window H.  Port of window.c:Fset_window_point.
    subroutine set_point_of_window(h, pos)
        type(handle_type), intent(in) :: h
        integer(int64), intent(in) :: pos
        type(status_type) :: status

        if (same_window(h, selected_frame%selected_window) .and. &
            same_handle(windows(h%index)%buffer, current_buffer())) then
            call goto_char(pos)
        else
            if (windows(h%index)%pointm%index == 0) call make_marker(windows(h%index)%pointm)
            call set_marker_restricted(windows(h%index)%pointm, pos, status, windows(h%index)%buffer)
        end if
    end subroutine set_point_of_window

    !> The index in `winner_point_alist' of BUFFER's entry, or 0.
    !> Not a port.
    integer function point_entry(buffer) result(k)
        type(handle_type), intent(in) :: buffer

        if (allocated(winner_point_alist)) then
            do k = 1, size(winner_point_alist)
                if (same_handle(winner_point_alist(k)%buffer, buffer)) return
            end do
        end if

        k = 0
    end function point_entry

    !> A new entry of `winner_point_alist' for BUFFER (current): its mark
    !> and whether the region is active.  Not a port.
    subroutine append_point_entry(buffer)
        type(handle_type), intent(in) :: buffer
        type(winner_point_type) :: entry

        entry%buffer = buffer
        entry%mark = mark_position()
        entry%mark_active = is_region_active()
        allocate (entry%windows(0), entry%points(0))
        if (.not. allocated(winner_point_alist)) allocate (winner_point_alist(0))
        winner_point_alist = [winner_point_alist, entry]
    end subroutine append_point_entry

    !> Record the points of the windows and the marks of their buffers in
    !> `winner_point_alist'.  Port of winner.el:winner-make-point-alist.
    subroutine make_winner_point_alist()
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: old, buffer
        type(status_type) :: status
        integer :: k, e

        old = current_buffer()
        if (allocated(winner_point_alist)) deallocate (winner_point_alist)
        allocate (winner_point_alist(0))
        list = window_list(MINIBUF_NO, status=status)

        do k = 1, size(list)
            buffer = windows(list(k)%index)%buffer
            e = point_entry(buffer)

            if (e == 0) then
                call set_buffer(buffer, status)
                call append_point_entry(buffer)
                e = size(winner_point_alist)
            end if

            associate (entry => winner_point_alist(e))
                entry%windows = [entry%windows, list(k)]
                entry%points = [entry%points, point_of_window(list(k))]
            end associate
        end do

        if (is_buffer_live(old)) call set_buffer(old, status)
    end subroutine make_winner_point_alist

    !> The point to restore for window WIN (the null handle for none)
    !> showing BUFFER, consulting (and possibly extending)
    !> `winner_point_alist'; FOUND is false if BUFFER no longer exists.
    !> Port of winner.el:winner-get-point.
    subroutine get_winner_point(buffer, win, pos, found)
        type(handle_type), intent(in) :: buffer, win
        integer(int64), intent(out) :: pos
        logical, intent(out) :: found
        type(handle_type) :: old
        type(status_type) :: status
        integer :: e, k

        pos = 0
        found = is_buffer_live(buffer)
        if (.not. found) return
        old = current_buffer()
        e = point_entry(buffer)

        if (e > 0) then
            associate (entry => winner_point_alist(e))
                do k = 1, size(entry%windows)
                    if (same_window(entry%windows(k), win)) then
                        pos = entry%points(k)
                        return
                    end if
                end do

                do k = 1, size(entry%windows)
                    if (entry%windows(k)%index == 0) then
                        pos = entry%points(k)
                        return
                    end if
                end do

                call set_buffer(buffer, status)
                pos = buffers(current)%pt
                entry%windows = [entry%windows, null_handle()]
                entry%points = [entry%points, pos]
            end associate
        else
            call set_buffer(buffer, status)
            pos = buffers(current)%pt
            call append_point_entry(buffer)

            associate (entry => winner_point_alist(size(winner_point_alist)))
                entry%windows = [entry%windows, null_handle()]
                entry%points = [entry%points, pos]
            end associate
        end if

        if (is_buffer_live(old)) call set_buffer(old, status)
    end subroutine get_winner_point

    !> Whether the buffer of window H is boring: named in
    !> `winner_boring_buffers' or matching `winner_boring_buffers_regexp'.
    !> Not a port.
    logical function is_boring(h)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: name
        integer(int64) :: bi
        type(status_type) :: status

        is_boring = .false.
        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (.not. status%ok()) return
        name = buffers(bi)%name

        if (allocated(winner_boring_buffers)) then
            is_boring = any(winner_boring_buffers == name)
            if (is_boring) return
        end if

        if (allocated(winner_boring_buffers_regexp)) is_boring = string_match(winner_boring_buffers_regexp, name, status) > 0
    end function is_boring

    !> Restore Winner configuration CONF: its windows, their points and
    !> their buffers' marks, then delete the windows whose buffers are dead
    !> or boring.  POSSIBLE is true if this is still a possible
    !> configuration.  Port of winner.el:winner-set.
    subroutine set_winner(conf, possible, status)
        type(winner_conf_type), intent(in) :: conf
        logical, intent(out) :: possible
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: chosen(:), list(:), xwins(:), live_buffers(:)
        logical, allocatable :: alive(:)
        type(handle_type) :: old, buffer
        integer(int64) :: pos
        logical :: found, keep
        integer :: k, n, e

        possible = .true.
        ! Possibly update `winner_point_alist'.
        allocate (alive(size(conf%data%windows)), live_buffers(0))

        do k = 1, size(conf%data%windows)
            buffer = conf%data%windows(k)%buffer
            call get_winner_point(buffer, null_handle(), pos, found)
            alive(k) = found

            if (found) then
                if (.not. any([(same_handle(live_buffers(n), buffer), n=1, size(live_buffers))])) then
                    live_buffers = [live_buffers, buffer]
                end if
            end if
        end do

        call set_winner_conf(conf%config, status)
        if (.not. status%ok()) return
        ! Windows to be deleted.
        allocate (xwins(0))
        ! Restore points.
        list = winner_sorted_window_list()

        do k = 1, size(list)
            keep = .false.

            if (k <= size(alive)) then
                if (alive(k)) then
                    call get_winner_point(windows(list(k)%index)%buffer, list(k), pos, found)

                    if (found) then
                        call set_point_of_window(list(k), pos)
                        keep = .not. is_boring(list(k))
                    end if
                end if
            end if

            ! Delete this window.
            if (.not. keep) xwins = [list(k), xwins]
        end do

        ! Restore marks (`select_enable_primary' is not ported: as when it
        ! is false).
        old = current_buffer()

        do k = 1, size(live_buffers)
            e = point_entry(live_buffers(k))
            if (e == 0) cycle
            call set_buffer(live_buffers(k), status)
            call set_mark(winner_point_alist(e)%mark)

            if (winner_point_alist(e)%mark_active) then
                call activate_mark()
            else
                call deactivate_mark()
            end if
        end do

        if (is_buffer_live(old)) call set_buffer(old, status)
        status = status_type()
        ! Delete windows, whose buffers are dead or boring.  Return true if
        ! this is still a possible configuration.
        if (size(xwins) == 0) return

        ! Delete all but one.
        do k = 2, size(xwins)
            call delete_window(xwins(k), status)
            if (.not. status%ok()) return
        end do

        call window_leaves(selected_frame%root_window, chosen)
        possible = size(chosen) > 1

        if (possible) then
            call delete_window(xwins(1), status)
        end if
    end subroutine set_winner

    ! ------------------------------------------------------------------
    ! Winner mode, its commands
    ! ------------------------------------------------------------------

    !> Toggle Winner mode on or off.
    !>
    !> Winner mode is a global minor mode that records the changes in
    !> the window configuration (i.e. how the frames are partitioned
    !> into windows) so that the changes can be "undone" using the
    !> command `undo_window_change'.  By default this one is bound to the key
    !> sequence C-c <left>.  If you change your mind (while undoing),
    !> you can press C-c <right> (calling `redo_window_change').
    !>
    !> If you use `tab_bar_mode', consider using `tab_bar_history_mode', as
    !> `winner_mode' is unaware of tab switching, and might turn the window
    !> configuration of the current tab to another's (old) window
    !> configuration.  `tab_bar_history_mode' provides tab-specific window
    !> configuration history avoiding this problem.  Port of
    !> winner.el:winner-mode.
    recursive subroutine winner_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%winner_mode%command(status)
    end subroutine winner_mode

    !> The body of Winner mode.  Port of winner.el:winner-mode (its body).
    recursive subroutine winner_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call load_winner()

        if (enable) then
            call window_configuration_change_hook%prepend(winner_change_fun)
            call post_command_hook%prepend(save_winner_old_configurations)
            call minibuffer_setup_hook%prepend(save_winner_unconditionally)
            winner_modified = .true.
            call save_winner_old_configurations(status)
        else
            call window_configuration_change_hook%remove(winner_change_fun)
            call post_command_hook%remove(save_winner_old_configurations)
            call minibuffer_setup_hook%remove(save_winner_unconditionally)
        end if
    end subroutine winner_mode_body

    !> Bind the default keys of `winner_mode_map' unless
    !> `winner_dont_bind_my_keys' (as loading winner.el does, the first
    !> time).  Port of winner.el:winner--set-dont-bind-my-keys (as
    !> `setopt' runs it when winner.el loads).
    subroutine load_winner()
        type(status_type) :: st

        if (winner_loaded) return
        winner_loaded = .true.

        if (winner_dont_bind_my_keys) then
            call keymap_set(winner_mode_map, "C-c <left>", value_type(), st)
            call keymap_set(winner_mode_map, "C-c <right>", value_type(), st)
        else
            ! Default bindings.
            call keymap_set(winner_mode_map, "C-c <left>", "undo_window_change", st)
            call keymap_set(winner_mode_map, "C-c <right>", "redo_window_change", st)
        end if
    end subroutine load_winner

    !> Switch back to an earlier window configuration saved by Winner mode.
    !> In other words, "undo" changes in window configuration.  Port of
    !> winner.el:winner-undo.
    recursive subroutine undo_window_change(status)
        type(status_type), intent(out) :: status
        type(command_type), pointer :: undo
        logical :: done
        character(len=24) :: counter, length

        status = status_type()

        if (.not. minor_modes%winner_mode%enabled()) then
            call set_error(status, FM_GENERIC_ERROR, "Winner mode is turned off")
            return
        end if

        undo => commands%undo_window_change

        if (.not. (is_same_command(last_command, undo) .and. undo_window_change_frame_set)) then
            ! Current configuration->stack.
            call save_winner_conditionally()
            undo_window_change_frame_set = .true.
            call make_winner_point_alist()
            call winner_ring()
            undo_window_change_counter = 0
            if (allocated(winner_undone_data)) deallocate (winner_undone_data)
            allocate (winner_undone_data(1))
            call winner_win_data(winner_undone_data(1))
        end if

        ! Starting at 1.
        undo_window_change_counter = undo_window_change_counter + 1
        call undo_window_change_this(done, status)
        if (.not. status%ok()) return

        if (done .and. .not. selected_frame%minibuffer_selected) then
            write (counter, "(i0)") undo_window_change_counter
            write (length, "(i0)") size(winner_ring_confs) - 1
            call message("Winner undo ("//trim(counter)//" / "//trim(length)//")")
        end if
    end subroutine undo_window_change

    !> Restore the configuration `undo_window_change_counter' back in the ring,
    !> discarding the ones that are no longer possible or show what was
    !> already passed; DONE false if there is none.  The heart of winner
    !> undo.  Port of winner.el:winner-undo-this.
    subroutine undo_window_change_this(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(winner_data_type) :: data
        type(winner_conf_type) :: dropped
        logical :: possible
        integer :: k

        done = .false.
        status = status_type()

        do
            if (undo_window_change_counter >= size(winner_ring_confs)) then
                call message("No further window configuration undo information")
                return
            end if

            ! If possible configuration
            call set_winner(winner_ring_confs(undo_window_change_counter + 1), possible, status)
            if (.not. status%ok()) return

            if (possible) then
                ! .. and new configuration
                call winner_win_data(data)

                if (.not. any([(is_winner_equal(data, winner_undone_data(k)), k=1, size(winner_undone_data))])) then
                    winner_undone_data = [winner_undone_data, data]
                    ! .. then everything is fine.
                    done = .true.
                    return
                end if
            end if

            ! Otherwise, discharge it (and try the next one).
            call ring_remove(undo_window_change_counter, dropped)
            call dropped%destroy()
        end do
    end subroutine undo_window_change_this

    !> Cancel the current sequence of `undo_window_change' commands.
    !> This restores the window configuration before that sequence.  If any
    !> other command has intervened, `redo_window_change' is no longer applicable;
    !> but the configuration can then be restored with `undo_window_change'.
    !> Port of winner.el:winner-redo.
    recursive subroutine redo_window_change(status)
        type(status_type), intent(out) :: status
        type(command_type), pointer :: undo
        type(winner_conf_type) :: conf
        logical :: possible

        status = status_type()
        undo => commands%undo_window_change

        if (.not. is_same_command(last_command, undo)) then
            call set_error(status, FM_USER_ERROR, format_message("Previous command was not a `undo_window_change'"))
            return
        end if

        if (minibuf_level == 0) then
            call ring_remove(0, conf)
            call set_winner(conf, possible, status)
            call conf%destroy()
        else
            call set_winner(winner_ring_confs(1), possible, status)
        end if

        if (.not. status%ok()) return
        if (.not. selected_frame%minibuffer_selected) call message("Winner undid undo")
    end subroutine redo_window_change

    !> Define Winner mode, its commands and keymaps (as loading winner.el
    !> does, but for the keys of `winner_mode_map').  Not a port.
    subroutine init_winner()
        type(status_type) :: st

        if (.not. allocated(winner_boring_buffers)) winner_boring_buffers = [character(len=256) :: "*Completions*"]
        call minor_modes%winner_mode%define("winner_mode", global=.true., hook=winner_mode_hook, keymap=.true., &
                                            body=winner_mode_body)
        winner_mode_map = minor_modes%winner_mode%map
        commands%winner_mode%execute => winner_mode
        commands%undo_window_change%execute => undo_window_change
        commands%redo_window_change%execute => redo_window_change
        if (is_keymap_live(winner_repeat_map)) call destroy_keymap(winner_repeat_map, st)
        call make_sparse_keymap(winner_repeat_map)
        call keymap_set(winner_repeat_map, "<left>", "undo_window_change", st)
        call keymap_set(winner_repeat_map, "<right>", "redo_window_change", st)
        call set_repeat_map(winner_repeat_map)
    end subroutine init_winner
end module winner_mod
