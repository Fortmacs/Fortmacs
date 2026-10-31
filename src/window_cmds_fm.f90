! window_cmds_fm.f90 -- selecting, splitting and deleting windows, and
! switching and killing buffers.
! Ported from GNU Emacs 31.1 src/window.c (Fselect_window), lisp/window.el
! (split-window, split-window-below, split-window-right, delete-window,
! delete-other-windows, other-window, switch-to-buffer,
! replace-buffer-in-windows), src/buffer.c (Fother_buffer, Fkill_buffer's
! interactive part) and lisp/simple.el (kill-buffer--possibly-save).
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of window_fm: the commands on the window tree (C-x 2, C-x 3,
!> C-x 0, C-x 1, C-x o) and on buffers (C-x b, C-x k).  The selected
!> window shows the current buffer: selecting a window makes its buffer
!> current with point at the window's point, and the window it leaves
!> keeps its point in `pointm'.  These need the frame (frame_fm), which
!> depends on window_fm, so they live in a submodule.
!>
!> Deferred: window parameters, side and atomic windows, dedicated
!> windows, `window_combination_resize', `split_window_keep_point' .false.,
!> `delete_window_choose_selected' other than `mru', other frames, the
!> window commands while the minibuffer is selected, buffer menus
!> (C-x C-b), `switch_to_buffer_preserve_window_point',
!> `kill_buffer_query_functions' and `kill_buffer_hook'.
submodule(window_fm) window_cmds_fm
    use commands_fm, only: commands
    use error_fm, only: format_message, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, FM_USER_ERROR
    use buffer_fm, only: cur, current_buffer, set_buffer, buffer_list, get_buffer, get_buffer_create, &
                         record_buffer, frame_seen_buffers, is_buffer_live, kill_buffer, is_buffer_modified, &
                         replace_buffer_in_windows_hook
    use editfns_fm, only: goto_char
    use marker_fm, only: marker_position
    use frame_fm, only: selected_frame, ensure_frame_windows
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use lisp_fm, only: list, value_type
    use minibuf_fm, only: completion_list_table
    use minibuffer_fm, only: read_buffer, completing_read, REQUIRE_MATCH_YES, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use files_fm, only: save_buffer
    implicit none(type, external)

contains

    !> An error for a window that is not live.  Not a port.
    subroutine not_live(status)
        type(status_type), intent(out) :: status

        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected live type(window_type)")
    end subroutine not_live

    !> The window commands can't run in the minibuffer yet.  Not a port.
    logical function in_minibuffer(status)
        type(status_type), intent(out) :: status

        in_minibuffer = selected_frame%minibuffer_selected

        if (in_minibuffer) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Window commands don't work in the minibuffer yet"))
        else
            status = status_type()
        end if
    end function in_minibuffer

    module subroutine select_window(h, status, norecord)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord
        type(handle_type) :: old
        integer(int64) :: bi
        logical :: record

        if (.not. is_window_live(h)) then
            call not_live(status)
            return
        end if

        record = .true.
        if (present(norecord)) record = .not. norecord
        status = status_type()
        old = selected_frame%selected_window

        if (.not. same_window(old, h)) then
            ! The window left keeps its buffer's point.
            if (is_window_live(old)) then
                call resolve_buffer(windows(old%index)%buffer, bi, status)
                if (status%ok()) call set_window_point(windows(old%index), buffers(bi)%pt, status)
            end if

            selected_frame%selected_window = h
            call set_buffer(windows(h%index)%buffer, status)
            if (.not. status%ok()) return
            call goto_char(window_point(windows(h%index)))
        else
            call set_buffer(windows(h%index)%buffer, status)
            if (.not. status%ok()) return
        end if

        if (record) then
            window_select_count = window_select_count + 1
            windows(h%index)%use_time = window_select_count
            call record_buffer(windows(h%index)%buffer)
        end if
    end subroutine select_window

    module function window_description(h) result(s)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: s
        character(len=20) :: num
        integer(int64) :: bi
        type(status_type) :: status

        write (num, "(i0)") h%index
        s = "#<window "//trim(num)
        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (status%ok()) s = s//" on "//buffers(bi)%name
        s = s//">"
    end function window_description

    recursive module integer function window_min_size(window, horizontal) result(n)
        type(handle_type), intent(in) :: window
        logical, intent(in) :: horizontal
        type(handle_type) :: sub

        n = 0
        sub = windows(window%index)%child

        if (is_window_valid(sub)) then
            if (windows(window%index)%horizontal .eqv. horizontal) then
                do while (is_window_valid(sub))
                    n = n + window_min_size(sub, horizontal)
                    sub = windows(sub%index)%next
                end do
            else
                do while (is_window_valid(sub))
                    n = max(n, window_min_size(sub, horizontal))
                    sub = windows(sub%index)%next
                end do
            end if
        else if (horizontal) then
            n = max(window_safe_min_width, window_min_width)
        else if (windows(window%index)%mini) then
            n = window_safe_min_height
        else
            ! A text line and the mode line.
            n = max(window_safe_min_height + 1, window_min_height)
        end if
    end function window_min_size

    module subroutine split_window(window, size, have_size, horizontal, new, status)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: size
        logical, intent(in) :: have_size, horizontal
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        type(handle_type) :: refer, w
        integer :: old_size, new_size, safe_size, frame_cols
        logical :: too_small
        integer(int64) :: bi

        if (.not. is_window_valid(window)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected valid type(window_type)")
            return
        end if

        if (horizontal) then
            old_size = windows(window%index)%total_cols
            safe_size = window_safe_min_width
        else
            old_size = windows(window%index)%total_lines
            safe_size = window_safe_min_height
        end if

        if (.not. have_size) then
            ! Give the new window half the size of WINDOW.
            new_size = old_size/2
            too_small = new_size + window_min_size(window, horizontal) > old_size
        else if (size >= 0) then
            ! SIZE non-negative specifies the new size of WINDOW.
            new_size = old_size - size
            too_small = size >= old_size .or. new_size < safe_size
        else
            ! SIZE negative specifies the size of the new window.
            new_size = -size
            too_small = new_size < safe_size .or. old_size - new_size < safe_size
        end if

        if (too_small) then
            call set_error(status, FM_GENERIC_ERROR, "Window "//window_description(window)//" too small for splitting")
            return
        end if

        ! The reference window: WINDOW if live, else the selected one.
        if (is_window_live(window)) then
            refer = window
        else
            refer = selected_frame%selected_window
        end if

        ! The selected window's point is its buffer's point.
        if (same_window(refer, selected_frame%selected_window)) then
            call resolve_buffer(windows(refer%index)%buffer, bi, status)
            if (status%ok()) call set_window_point(windows(refer%index), buffers(bi)%pt, status)
        end if

        ! (A copy: WINDOW may be the root window, which this updates.)
        w = window
        call split_window_internal(w, horizontal, new_size, selected_frame%root_window, new, status, refer)
        if (.not. status%ok()) return

        if (is_window_valid(windows(window%index)%child)) then
            ! The windows of an internal WINDOW share what is left.
            frame_cols = windows(selected_frame%root_window%index)%total_cols

            associate (w => windows(window%index))
                call resize_window_tree(window, w%top_line, w%left_col, w%total_lines, w%total_cols, frame_cols)
            end associate
        end if
    end subroutine split_window

    !> Split WINDOW_TO_SPLIT into two windows, one above the other.
    !> WINDOW_TO_SPLIT defaults to the selected window if omitted.
    !> The newly created window will be below WINDOW_TO_SPLIT and will show
    !> the same buffer as WINDOW_TO_SPLIT.
    !>
    !> If optional argument SIZE is omitted, both windows get the
    !> same height, or close to it.  If SIZE is positive, the upper
    !> (selected) window gets SIZE lines.  If SIZE is negative, the
    !> lower (new) window gets -SIZE lines.  Interactively, SIZE is
    !> the prefix numeric argument.
    !>
    !> Both windows get the same value of point as the WINDOW_TO_SPLIT.
    !> Port of window.el:split-window-below (the command;
    !> `split_window_keep_point' is t).
    subroutine cmd_split_window_below(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new

        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_arg)), &
                          allocated(current_prefix_arg%a), .false., new, status)
    end subroutine cmd_split_window_below

    !> Split WINDOW_TO_SPLIT into two side-by-side windows.
    !> WINDOW_TO_SPLIT defaults to the selected window if omitted.
    !> The newly created window will be to the right of WINDOW_TO_SPLIT and
    !> will show the same buffer as WINDOW_TO_SPLIT.
    !>
    !> If optional argument SIZE is omitted, both windows get the
    !> same width, or close to it.  If SIZE is positive, the left-hand
    !> (selected) window gets SIZE columns.  If SIZE is negative, the
    !> right-hand (new) window gets -SIZE columns.  Here, SIZE includes
    !> the width of the divider column to the window's right.
    !> Interactively, SIZE is the prefix numeric argument.
    !> Port of window.el:split-window-right (the command).
    subroutine cmd_split_window_right(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new

        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_arg)), &
                          allocated(current_prefix_arg%a), .true., new, status)
    end subroutine cmd_split_window_right

    !> Run `unshow_buffer' for the live windows of the tree under `h',
    !> about to be deleted (window.c:delete_all_child_windows and
    !> Fdelete_window_internal).  Not a port: a helper for those.
    subroutine unshow_leaves(h)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: leaves(:)
        integer :: k

        call window_leaves(h, leaves)

        do k = 1, size(leaves)
            if (is_buffer_live(windows(leaves(k)%index)%buffer)) call unshow_buffer(leaves(k))
        end do
    end subroutine unshow_leaves

    module subroutine delete_window(window, status)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: heir, mru
        type(handle_type), allocatable :: leaves(:)
        integer :: k, frame_cols
        logical :: was_selected

        status = status_type()

        if (.not. is_window_valid(window)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected valid type(window_type)")
            return
        end if

        if (same_window(window, selected_frame%root_window) .or. windows(window%index)%mini) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to delete minibuffer or sole ordinary window")
            return
        end if

        was_selected = same_window(window, selected_frame%selected_window)
        frame_cols = windows(selected_frame%root_window%index)%total_cols
        call remove_window_from_tree(window, selected_frame%root_window, heir, frame_cols)
        call unshow_leaves(window)
        call delete_window_tree(window)
        if (.not. was_selected) return
        ! delete-window-choose-selected `mru': the most recently used window.
        call window_leaves(selected_frame%root_window, leaves)
        mru = leaves(1)

        do k = 2, size(leaves)
            if (windows(leaves(k)%index)%use_time > windows(mru%index)%use_time) mru = leaves(k)
        end do

        selected_frame%selected_window = null_handle()
        call select_window(mru, status)
    end subroutine delete_window

    !> Delete specified WINDOW.
    !> WINDOW must be a valid window and defaults to the selected one.
    !>
    !> An error condition occurs if WINDOW is the only window on its frame.
    !>
    !> If WINDOW is the selected window on its frame, choose some other
    !> window as that frame's selected window: the most recently used one.
    !> Port of window.el:delete-window (the command; no window parameters).
    subroutine cmd_delete_window(status)
        type(status_type), intent(out) :: status

        if (in_minibuffer(status)) return
        call delete_window(selected_frame%selected_window, status)
    end subroutine cmd_delete_window

    module subroutine delete_other_windows(window, status)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: old_root, parent
        integer :: top, left, lines, cols

        if (.not. is_window_valid(window)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected valid type(window_type)")
            return
        end if

        status = status_type()
        old_root = selected_frame%root_window
        if (same_window(window, old_root)) return

        associate (r => windows(old_root%index))
            top = r%top_line
            left = r%left_col
            lines = r%total_lines
            cols = r%total_cols
        end associate

        ! Take WINDOW out of its combination, then free the rest.
        associate (w => windows(window%index))
            parent = w%parent

            if (is_window_valid(w%prev)) then
                windows(w%prev%index)%next = w%next
            else
                windows(parent%index)%child = w%next
            end if

            if (is_window_valid(w%next)) windows(w%next%index)%prev = w%prev
            w%parent = null_handle()
            w%next = null_handle()
            w%prev = null_handle()
        end associate

        call unshow_leaves(old_root)
        call delete_window_tree(old_root)
        selected_frame%root_window = window
        call resize_window_tree(window, top, left, lines, cols, left + cols)
        if (is_window_live(window)) call select_window(window, status)
    end subroutine delete_other_windows

    !> Make WINDOW fill its frame.
    !> WINDOW must be a valid window and defaults to the selected one.
    !> Port of window.el:delete-other-windows (the command; no window
    !> parameters).
    subroutine cmd_delete_other_windows(status)
        type(status_type), intent(out) :: status

        if (in_minibuffer(status)) return
        call delete_other_windows(selected_frame%selected_window, status)
    end subroutine cmd_delete_other_windows

    module subroutine other_window(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: leaves(:)
        integer :: k, n, at

        if (in_minibuffer(status)) return
        call window_leaves(selected_frame%root_window, leaves)
        n = size(leaves)
        if (n == 0) return
        at = 1

        do k = 1, n
            if (same_window(leaves(k), selected_frame%selected_window)) at = k
        end do

        call select_window(leaves(int(modulo(at - 1 + count, int(n, int64))) + 1), status)
    end subroutine other_window

    !> Select another window in cyclic ordering of windows.
    !> COUNT specifies the number of windows to skip, starting with the
    !> selected window, before making the selection.  If COUNT is
    !> positive, skip COUNT windows forwards.  If COUNT is negative,
    !> skip -COUNT windows backwards.  COUNT zero means do not skip any
    !> window, so select the selected window.  In an interactive call,
    !> COUNT is the numeric prefix argument.
    !> Port of window.el:other-window (the command; one frame).
    subroutine cmd_other_window(status)
        type(status_type), intent(out) :: status

        call other_window(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_other_window

    !> Select another window in the reverse cyclic ordering of windows.
    !> COUNT specifies the number of windows to skip, (by default) backward,
    !> starting with the selected window, before making the selection.  Like
    !> `other_window', but moves in the opposite direction.  Port of
    !> window.el:other-window-backward (the command; one frame).
    subroutine cmd_other_window_backward(status)
        type(status_type), intent(out) :: status

        call other_window(-prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_other_window_backward

    !> Whether buffer `b' is shown in a window of the frame.  Not a port.
    logical function is_buffer_visible(b)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: leaves(:)
        integer :: k

        is_buffer_visible = .false.
        if (.not. is_window_valid(selected_frame%root_window)) return
        call window_leaves(selected_frame%root_window, leaves)

        do k = 1, size(leaves)
            associate (wb => windows(leaves(k)%index)%buffer)
                if (wb%index == b%index .and. wb%generation == b%generation) then
                    is_buffer_visible = .true.
                    return
                end if
            end associate
        end do
    end function is_buffer_visible

    module function other_buffer(buffer, visible_ok) result(h)
        type(handle_type), intent(in) :: buffer
        logical, intent(in), optional :: visible_ok
        type(handle_type) :: h
        type(handle_type), allocatable :: seen(:), all(:)
        type(handle_type) :: notsogood
        type(status_type) :: status
        integer :: k
        logical :: vok

        vok = .false.
        if (present(visible_ok)) vok = visible_ok
        notsogood = null_handle()
        ! Consider buffers that have been seen in the frame first.
        seen = frame_seen_buffers()

        do k = 1, size(seen)
            if (.not. candidate_buffer(seen(k), buffer)) cycle

            if (vok .or. .not. is_buffer_visible(seen(k))) then
                h = seen(k)
                return
            else if (notsogood%index == 0) then
                notsogood = seen(k)
            end if
        end do

        ! Consider table of all buffers next.
        all = buffer_list()

        do k = 1, size(all)
            if (.not. candidate_buffer(all(k), buffer)) cycle

            if (vok .or. .not. is_buffer_visible(all(k))) then
                h = all(k)
                return
            else if (notsogood%index == 0) then
                notsogood = all(k)
            end if
        end do

        if (notsogood%index /= 0) then
            h = notsogood
        else
            call get_buffer_create("*scratch*", h, status)
        end if
    end function other_buffer

    !> Whether `b' is live, not `buffer' and not internal (its name does
    !> not start with a space).  Port of buffer.c:candidate_buffer.
    logical function candidate_buffer(b, buffer)
        type(handle_type), intent(in) :: b, buffer

        candidate_buffer = .false.
        if (.not. is_buffer_live(b)) return
        if (b%index == buffer%index .and. b%generation == buffer%generation) return

        if (len(buffers(b%index)%name) > 0) then
            if (buffers(b%index)%name(1:1) == " ") return
        end if

        candidate_buffer = .true.
    end function candidate_buffer

    module subroutine switch_to_buffer(name, status, norecord, force_same_window)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord, force_same_window
        type(handle_type) :: buffer, sw
        integer(int64) :: start, point
        logical :: force, norec, entry
        integer :: k

        force = .false.
        if (present(force_same_window)) force = force_same_window
        norec = .false.
        if (present(norecord)) norec = norecord
        ! window-normalize-buffer-to-switch-to.
        buffer = get_buffer(name)

        if (.not. is_buffer_live(buffer)) then
            call get_buffer_create(name, buffer, status)
            if (.not. status%ok()) return
        end if

        call ensure_frame_windows()
        sw = selected_frame%selected_window
        status = status_type()

        if (same_handle(buffer, windows(sw%index)%buffer)) then
            ! Don't call set-window-buffer if it's not needed since it
            ! might signal an error (e.g. if the window is dedicated).
            continue
        else if (windows(sw%index)%mini) then
            if (force) then
                call set_error(status, FM_USER_ERROR, "Cannot switch buffers in minibuffer window")
                return
            end if

            ! (pop-to-buffer buffer norecord): NORECORD is the ACTION, a
            ! one that is .true. meaning `inhibit_same_window'.
            call pop_to_buffer(buffer, status, table=display_action_table_type(inhibit_same_window=norec))
            if (.not. status%ok()) return
        else if (windows(sw%index)%dedicated == DEDICATED_STRONG) then
            if (force) then
                call set_error(status, FM_USER_ERROR, "Cannot switch buffers in a dedicated window")
                return
            end if

            ! (pop-to-buffer buffer norecord): NORECORD is the ACTION, a
            ! one that is .true. meaning `inhibit_same_window'.
            call pop_to_buffer(buffer, status, table=display_action_table_type(inhibit_same_window=norec))
            if (.not. status%ok()) return
        else
            ! `switch_to_buffer_preserve_window_point' is t.
            entry = .false.

            if (allocated(windows(sw%index)%prev_buffers)) then
                do k = 1, size(windows(sw%index)%prev_buffers)
                    if (same_handle(windows(sw%index)%prev_buffers(k)%buffer, buffer)) then
                        call marker_position(windows(sw%index)%prev_buffers(k)%start, start, status)
                        if (status%ok()) call marker_position(windows(sw%index)%prev_buffers(k)%point, point, status)
                        entry = status%ok()
                        exit
                    end if
                end do
            end if

            status = status_type()
            ! Make sure quitting the window works.
            call display_buffer_record_window(QR_OTHER, sw, buffer)
            call set_window_buffer(sw, buffer, status)
            if (.not. status%ok()) return

            if (entry) then
                ! Try to restore start and point of buffer in the selected
                ! window (Bug#4041).
                call set_window_start(windows(sw%index), start, status, noforce=.true.)
                if (.not. status%ok()) return
                call set_buffer(buffer, status)
                if (.not. status%ok()) return
                call goto_char(point)
                if (.not. status%ok()) return
            end if
        end if

        if (.not. norec) then
            call select_window(selected_frame%selected_window, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(buffer, status)
    end subroutine switch_to_buffer

    !> Display buffer BUFFER_OR_NAME in the selected window.
    !>
    !> If called interactively, read the buffer name using `read_buffer'.
    !> The variable `confirm_nonexistent_file_or_buffer' determines
    !> whether to request confirmation before creating a new buffer.
    !>
    !> If BUFFER_OR_NAME is a string that does not identify an existing
    !> buffer, create a buffer with that name.  If BUFFER_OR_NAME is
    !> empty, switch to the buffer returned by `other_buffer'.
    !> Port of window.el:switch-to-buffer (the command, with
    !> read-buffer-to-switch; `confirm_nonexistent_file_or_buffer' is
    !> `after_completion').
    recursive subroutine cmd_switch_to_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: other
        integer(int64) :: bi
        logical :: force_same_window

        call ensure_frame_windows()
        force_same_window = .false.

        associate (w => windows(selected_frame%selected_window%index))
            if (w%mini) then
                continue
            else if (w%dedicated /= DEDICATED_STRONG) then
                force_same_window = .true.
            else
                ! `switch_to_buffer_in_dedicated_window' is nil.
                call set_error(status, FM_USER_ERROR, "Cannot switch buffers in a dedicated window")
                return
            end if
        end associate

        if (in_minibuffer(status)) return
        other = other_buffer(current_buffer())
        call resolve_buffer(other, bi, status)
        if (.not. status%ok()) return
        call read_buffer("Switch to buffer: ", name, status, def=buffers(bi)%name, &
                         require_match=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, except=current_buffer())
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(bi)%name
        call switch_to_buffer(name, status, force_same_window=force_same_window)
    end subroutine cmd_switch_to_buffer

    !> Ask the user to confirm killing of a modified BUFFER.
    !> If the user confirms, optionally save BUFFER that is about to be
    !> killed.  `ok' is false if the user says no.  Port of
    !> simple.el:kill-buffer--possibly-save (read-multiple-choice with long
    !> answers).
    recursive subroutine kill_buffer_possibly_save(buffer, ok, status)
        type(handle_type), intent(in) :: buffer
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer
        type(handle_type) :: saved
        integer(int64) :: bi

        ok = .false.
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return
        call completing_read("Buffer "//buffers(bi)%name//" modified; kill anyway? (yes, no, save and then kill) ", &
                             completion_list_table(list(value_type(text="yes"), value_type(text="no"), &
                                                        value_type(text="save and then kill"))), &
                             answer, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        if (answer == "no") return
        ok = .true.

        if (answer /= "yes") then
            saved = current_buffer()
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call save_buffer(1_int64, .true., status)
            if (is_buffer_live(saved)) call set_buffer(saved, status)
        end if
    end subroutine kill_buffer_possibly_save

    !> Kill the buffer specified by BUFFER_OR_NAME.
    !> The argument may be a buffer or the name of an existing buffer.
    !> Argument omitted means kill the current buffer.
    !>
    !> A modified buffer visiting a file is killed only after asking (and
    !> perhaps saving it).  Port of buffer.c:Fkill_buffer (the command:
    !> "bKill buffer: ", and kill-buffer--possibly-save).
    recursive subroutine cmd_kill_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: buf
        integer(int64) :: bi
        logical :: modified, ok

        if (in_minibuffer(status)) return
        call read_buffer("Kill buffer: ", name, status, def=buffers(cur)%name, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(cur)%name
        buf = get_buffer(name)

        if (.not. is_buffer_live(buf)) then
            call set_error(status, FM_GENERIC_ERROR, "No such buffer "//name)
            return
        end if

        call resolve_buffer(buf, bi, status)
        call is_buffer_modified(buf, modified, status)

        if (modified .and. allocated(buffers(bi)%filename)) then
            call kill_buffer_possibly_save(buf, ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if

        call kill_buffer(buf, status)
    end subroutine cmd_kill_buffer

    module subroutine syms_of_window_cmds()
        commands%split_window_below%run => cmd_split_window_below
        commands%split_window_right%run => cmd_split_window_right
        commands%delete_window%run => cmd_delete_window
        commands%delete_other_windows%run => cmd_delete_other_windows
        commands%other_window%run => cmd_other_window
        commands%other_window_backward%run => cmd_other_window_backward
        commands%switch_to_buffer%run => cmd_switch_to_buffer
        commands%kill_buffer%run => cmd_kill_buffer
    end subroutine syms_of_window_cmds
end submodule window_cmds_fm
