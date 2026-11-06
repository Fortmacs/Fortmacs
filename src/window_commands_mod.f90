! window_commands_mod.f90 -- selecting, splitting and deleting windows, and
! switching and closing buffers.
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

!> Part of window_mod: the commands on the window tree (C-x 2, C-x 3,
!> C-x 0, C-x 1, C-x o) and on buffers (C-x b, C-x k).  The selected
!> window shows the current buffer: selecting a window makes its buffer
!> current with point at the window's point, and the window it leaves
!> keeps its point in `pointm'.  These need the frame (frame_mod), which
!> depends on window_mod, so they live in a submodule.
!>
!> Deferred: window parameters other than those of side windows, atomic
!> windows, dedicated windows, `window_combination_resize', `split_window_keep_point' .false.,
!> `delete_window_choose_selected' other than `mru', other frames, the
!> window commands while the minibuffer is selected, buffer menus
!> (C-x C-b), `switch_to_buffer_preserve_window_point',
!> `close_buffer_query_functions' and `close_buffer_hook'.
submodule(window_mod) window_commands_mod
    use commands_mod, only: commands
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: substitute_quotes, set_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE, USER_ERROR_CODE
    use buffer_mod, only: current, current_buffer, set_buffer, buffer_list, get_buffer, get_or_make_buffer, &
                          record_buffer, frame_seen_buffers, is_buffer_live, close_buffer, is_buffer_modified, &
                          replace_buffer_in_windows_hook
    use editing_functions_mod, only: goto_character
    use marker_mod, only: marker_position
    use frame_mod, only: frame_type, selected_frame, ensure_frame_windows, window_frame, get_frame, select_frame
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use value_mod, only: list, value_type
    use minibuffer_reading_mod, only: completion_list_table
    use minibuffer_mod, only: read_buffer, completing_read, REQUIRE_MATCH_YES, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use files_mod, only: save_buffer
    use keyboard_mod, only: is_executing_keyboard_macro
    use display_engine_mod, only: message
    implicit none(type, external)

contains

    !> An error for a window that is not live.  Not a port.
    subroutine not_live(status)
        type(status_type), intent(out) :: status

        call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected live type(window_type)")
    end subroutine not_live

    !> The window commands can't run in the minibuffer yet.  Not a port.
    logical function in_minibuffer(status)
        type(status_type), intent(out) :: status

        in_minibuffer = selected_frame%minibuffer_selected

        if (in_minibuffer) then
            call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Window commands don't work in the minibuffer yet"))
        else
            status = status_type()
        end if
    end function in_minibuffer

    recursive module subroutine select_window(h, status, norecord)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord
        type(handle_type) :: old, frame
        type(frame_type), pointer :: f
        integer(int64) :: bi
        logical :: record

        if (.not. is_window_live(h)) then
            call not_live(status)
            return
        end if

        ! A window on another frame becomes its frame's selected window,
        ! and that frame is selected (which selects the window).
        frame = window_frame(h)

        if (frame%index /= 0 .and. .not. same_handle(frame, selected_frame%handle)) then
            f => get_frame(frame)
            if (.not. windows(h%index)%mini) f%selected_window = h
            call select_frame(frame, status, norecord)
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
                if (status%ok()) call set_window_point(windows(old%index), buffers(bi)%point, status)
            end if

            selected_frame%selected_window = h
            call set_buffer(windows(h%index)%buffer, status)
            if (.not. status%ok()) return
            call goto_character(window_point(windows(h%index)))
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
        character(len=20) :: number
        integer(int64) :: bi
        type(status_type) :: status

        write (number, "(i0)") h%index
        s = "#<window "//trim(number)
        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (status%ok()) s = s//" on "//buffers(bi)%name
        s = s//">"
    end function window_description

    recursive module integer function window_minimum_size(window, horizontal, ignore) result(n)
        type(handle_type), intent(in) :: window
        logical, intent(in) :: horizontal
        logical, intent(in), optional :: ignore
        type(handle_type) :: sub
        logical :: ignored

        n = 0
        ignored = .false.
        if (present(ignore)) ignored = ignore
        sub = windows(window%index)%child

        if (is_window_valid(sub)) then
            if (windows(window%index)%horizontal .eqv. horizontal) then
                do while (is_window_valid(sub))
                    n = n + window_minimum_size(sub, horizontal, ignored)
                    sub = windows(sub%index)%next
                end do
            else
                do while (is_window_valid(sub))
                    n = max(n, window_minimum_size(sub, horizontal, ignored))
                    sub = windows(sub%index)%next
                end do
            end if
        else if (horizontal) then
            n = window_safe_minimum_width
            if (.not. ignored) n = max(n, window_minimum_width)
        else if (windows(window%index)%mini) then
            n = window_safe_minimum_height
        else
            ! A text line and the mode line.
            n = window_safe_minimum_height + 1
            if (.not. ignored) n = max(n, window_minimum_height)
        end if
    end function window_minimum_size

    !> The number of lines (columns with HORIZONTAL) the windows of
    !> combination PARENT can give a new window among them: their total
    !> size divided by their number plus 1 (no window is fixed-size).
    !> Port of window.el:window--combination-resizable.
    integer function window_combination_resizable(parent, horizontal) result(n)
        type(handle_type), intent(in) :: parent
        logical, intent(in) :: horizontal
        type(handle_type) :: sibling
        integer :: number, total

        number = 0
        total = 0
        sibling = windows(parent%index)%child

        do while (is_window_valid(sibling))
            number = number + 1

            if (horizontal) then
                total = total + windows(sibling%index)%total_columns
            else
                total = total + windows(sibling%index)%total_lines
            end if

            sibling = windows(sibling%index)%next
        end do

        n = total/(number + 1)
    end function window_combination_resizable

    !> Return largest number of windows vertically arranged within WINDOW.
    !> If HORIZONTAL is true, return the largest number of windows
    !> horizontally arranged within WINDOW.  Port of
    !> window.el:window-combinations (no fixed-size windows).
    recursive integer function window_combinations(window, horizontal) result(count)
        type(handle_type), intent(in) :: window
        logical, intent(in) :: horizontal
        type(handle_type) :: child

        child = windows(window%index)%child

        if (.not. is_window_valid(child)) then
            ! If WINDOW is live, return 1.
            count = 1
        else if (windows(window%index)%horizontal .eqv. horizontal) then
            ! If WINDOW is iso-combined, return the sum of the values for
            ! all child windows of WINDOW.
            count = 0

            do while (is_window_valid(child))
                count = count + window_combinations(child, horizontal)
                child = windows(child%index)%next
            end do
        else
            ! If WINDOW is not iso-combined, return the maximum value of
            ! any child window of WINDOW.
            count = 1

            do while (is_window_valid(child))
                count = max(count, window_combinations(child, horizontal))
                child = windows(child%index)%next
            end do
        end if
    end function window_combinations

    !> Whether the window before or after `h' among its siblings is a
    !> side window.  Not a port.
    logical function has_side_sibling(h)
        type(handle_type), intent(in) :: h

        has_side_sibling = .false.

        associate (w => windows(h%index))
            if (is_window_valid(w%previous)) then
                if (windows(w%previous%index)%window_side /= WINDOW_SIDE_NONE) has_side_sibling = .true.
            end if

            if (is_window_valid(w%next)) then
                if (windows(w%next%index)%window_side /= WINDOW_SIDE_NONE) has_side_sibling = .true.
            end if
        end associate
    end function has_side_sibling

    module subroutine split_window(window, size, has_size, horizontal, new, status, before, side_resize)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: size
        logical, intent(in) :: has_size, horizontal
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: before, side_resize
        type(handle_type) :: refer, w, parent, sub
        integer :: old_size, new_size, safe_size, frame_columns, parent_size, side
        logical :: too_small, above, for_side, limit, resize, new_parent
        real(real64) :: new_normal
        integer(int64) :: bi

        if (.not. is_window_valid(window)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected valid type(window_type)")
            return
        end if

        above = .false.
        if (present(before)) above = before
        for_side = .false.
        if (present(side_resize)) for_side = side_resize
        call check_window_sides(status)
        if (.not. status%ok()) return
        ! (A copy: WINDOW may be the root window, which this updates.)
        w = window
        ! `window_combination_limit' t: make a new parent window.
        limit = .false.

        if (.not. ignore_window_parameters) then
            ! If WINDOW's frame has a side window and WINDOW specifies the
            ! frame's root window, split the frame's main window instead
            ! (Bug#73627).
            if (same_window(w, selected_frame%root_window)) then
                if (is_window_valid(window_with_side(WINDOW_SIDE_NONE, .false.))) w = window_main_window()
            end if

            if (.not. for_side .and. windows(w%index)%window_side /= WINDOW_SIDE_NONE) then
                ! If WINDOW is a side window or its first or last child is a
                ! side window, an error condition occurs unless
                ! `window_combination_resize' equals `side'.
                call set_error(status, GENERIC_ERROR_CODE, "Cannot split side window or parent of side window")
                return
            else if (.not. for_side .and. has_side_sibling(w)) then
                ! If WINDOW has a side window sibling, make a new parent.
                limit = .true.
            end if
        end if

        side = windows(w%index)%window_side
        parent = windows(w%index)%parent
        ! Resizing the other windows of WINDOW's combination makes sense
        ! in iso-combinations only.
        resize = for_side .and. side /= WINDOW_SIDE_NONE .and. .not. limit

        if (resize) then
            resize = .false.
            if (is_window_valid(parent)) resize = windows(parent%index)%horizontal .eqv. horizontal
        end if

        if (horizontal) then
            old_size = windows(w%index)%total_columns
            safe_size = window_safe_minimum_width
        else
            old_size = windows(w%index)%total_lines
            safe_size = window_safe_minimum_height
        end if

        parent_size = old_size

        if (is_window_valid(parent)) then
            if (horizontal) then
                parent_size = windows(parent%index)%total_columns
            else
                parent_size = windows(parent%index)%total_lines
            end if
        end if

        if (.not. has_size) then
            if (resize) then
                ! When resizing try to give the new window the average size
                ! of a window in its combination.
                if (horizontal) then
                    new_size = max(min(parent_size - window_minimum_size(parent, horizontal), &
                                       window_combination_resizable(parent, horizontal)), &
                                   max(window_minimum_width, window_safe_minimum_width))
                else
                    new_size = max(min(parent_size - window_minimum_size(parent, horizontal), &
                                       window_combination_resizable(parent, horizontal)), &
                                   max(window_minimum_height, window_safe_minimum_height))
                end if

                too_small = .not. is_window_sizable(parent, -new_size, horizontal, .false.)
            else
                ! Give the new window half the size of WINDOW.
                new_size = old_size/2
                too_small = new_size + window_minimum_size(w, horizontal) > old_size
            end if
        else if (size >= 0) then
            ! SIZE non-negative specifies the new size of WINDOW.
            new_size = old_size - size
            too_small = size >= old_size .or. new_size < safe_size
        else
            ! SIZE negative specifies the size of the new window.
            new_size = -size
            too_small = .false.
            if (.not. resize) too_small = new_size < safe_size .or. old_size - new_size < safe_size
        end if

        if (too_small) then
            call set_error(status, GENERIC_ERROR_CODE, "Window "//window_description(w)//" too small for splitting")
            return
        end if

        if (resize .and. has_size) then
            ! If we cannot resize the parent give up.
            if (.not. is_window_sizable(parent, -new_size, horizontal, .false.)) then
                call set_error(status, GENERIC_ERROR_CODE, "Window "//window_description(parent)//" too small for splitting")
                return
            end if
        end if

        ! The reference window: WINDOW if live, else the selected one.
        if (is_window_live(w)) then
            refer = w
        else
            refer = selected_frame%selected_window
        end if

        ! The selected window's point is its buffer's point.
        if (same_window(refer, selected_frame%selected_window)) then
            call resolve_buffer(windows(refer%index)%buffer, bi, status)
            if (status%ok()) call set_window_point(windows(refer%index), buffers(bi)%point, status)
        end if

        ! A new parent window is needed either because we want to nest or
        ! because WINDOW is not iso-combined.
        new_parent = limit .or. .not. is_window_valid(parent)
        if (.not. new_parent) new_parent = .not. (windows(parent%index)%horizontal .eqv. horizontal)

        if (resize) then
            ! Try to get space from WINDOW's siblings.
            if (has_size) then
                new_normal = real(new_size, real64)/parent_size
            else
                new_normal = 1.0_real64/(1 + window_combinations(parent, horizontal))
            end if

            call window_resize_reset(selected_frame%root_window, horizontal)
            call window_resize_child_windows(parent, -new_size, horizontal, .false.)
            sub = windows(parent%index)%child

            do while (is_window_valid(sub))
                if (horizontal) then
                    windows(sub%index)%normal_columns = windows(sub%index)%normal_columns*(1.0_real64 - new_normal)
                else
                    windows(sub%index)%normal_lines = windows(sub%index)%normal_lines*(1.0_real64 - new_normal)
                end if

                sub = windows(sub%index)%next
            end do

            call split_window_internal(w, horizontal, new_size, selected_frame%root_window, new, status, refer, &
                                       new_normal, before=above, resize=.true.)
        else if (has_size) then
            ! Make the new window's normal size its share of WINDOW, or of
            ! WINDOW's parent when WINDOW is in a combination of that kind.
            if (new_parent) parent_size = old_size
            call split_window_internal(w, horizontal, new_size, selected_frame%root_window, new, status, refer, &
                                       real(new_size, real64)/parent_size, before=above, new_parent=limit)
        else
            call split_window_internal(w, horizontal, new_size, selected_frame%root_window, new, status, refer, &
                                       before=above, new_parent=limit)
        end if

        if (.not. status%ok()) return

        if (for_side) then
            ! We made a new side window.
            if (side == WINDOW_SIDE_NONE) then
                if (.not. horizontal .and. above) then
                    side = WINDOW_SIDE_TOP
                else if (.not. horizontal) then
                    side = WINDOW_SIDE_BOTTOM
                else if (above) then
                    side = WINDOW_SIDE_LEFT
                else
                    side = WINDOW_SIDE_RIGHT
                end if
            end if

            windows(new%index)%window_side = side

            if (new_parent .and. windows(w%index)%window_side /= WINDOW_SIDE_NONE) then
                ! We've been splitting a side root window.  Give the new
                ! parent the same window_side parameter.
                windows(windows(new%index)%parent%index)%window_side = side
            end if
        end if

        if (is_window_valid(windows(w%index)%child)) then
            ! The windows of an internal WINDOW share what is left.
            frame_columns = windows(selected_frame%root_window%index)%total_columns

            associate (ww => windows(w%index))
                call resize_window_tree(w, ww%top_line, ww%left_column, ww%total_lines, ww%total_columns, frame_columns)
            end associate
        end if

        call execute_window_scroll_functions(new, status)
        if (.not. status%ok()) return
        call check_window_sides(status)
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
    subroutine execute_split_window_below(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new

        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_argument)), &
                          current_prefix_argument%kind /= PREFIX_NONE, .false., new, status)
    end subroutine execute_split_window_below

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
    subroutine execute_split_window_right(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new

        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_argument)), &
                          current_prefix_argument%kind /= PREFIX_NONE, .true., new, status)
    end subroutine execute_split_window_right

    module subroutine split_window_below(new, status)
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status

        call split_window(selected_frame%selected_window, 0, .false., .false., new, status)
    end subroutine split_window_below

    module subroutine split_window_right(new, status)
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status

        call split_window(selected_frame%selected_window, 0, .false., .true., new, status)
    end subroutine split_window_right

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
        integer :: k, frame_columns
        logical :: was_selected

        status = status_type()

        if (.not. is_window_valid(window)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected valid type(window_type)")
            return
        end if

        call check_window_sides(status)
        if (.not. status%ok()) return

        if (same_window(window, selected_frame%root_window) .or. windows(window%index)%mini) then
            call set_error(status, GENERIC_ERROR_CODE, "Attempt to delete minibuffer or sole ordinary window")
            return
        end if

        if (.not. ignore_window_parameters) then
            if (same_window(window, window_main_window())) then
                call set_error(status, GENERIC_ERROR_CODE, "Attempt to delete main window of frame "//selected_frame%name)
                return
            end if
        end if

        was_selected = same_window(window, selected_frame%selected_window)
        frame_columns = windows(selected_frame%root_window%index)%total_columns
        call remove_window_from_tree(window, selected_frame%root_window, heir, frame_columns)
        call unshow_leaves(window)
        call delete_window_tree(window)

        if (was_selected) then
            ! delete-window-choose-selected `mru': the most recently used
            ! window that is neither dedicated nor an "other" window not to
            ! use.
            call window_leaves(selected_frame%root_window, leaves)
            mru = null_handle()

            do k = 1, size(leaves)
                if (windows(leaves(k)%index)%dedicated /= DEDICATED_NONE) cycle
                if (is_window_no_other(leaves(k))) cycle

                if (mru%index == 0) then
                    mru = leaves(k)
                else if (windows(leaves(k)%index)%use_time > windows(mru%index)%use_time) then
                    mru = leaves(k)
                end if
            end do

            if (mru%index == 0) then
                ! The frame's first window (delete-window-internal's
                ! choice), unless it is an "other" window not to use and
                ! another one is not.
                mru = leaves(1)

                if (is_window_no_other(mru)) then
                    do k = 1, size(leaves)
                        if (.not. is_window_no_other(leaves(k))) then
                            mru = leaves(k)
                            exit
                        end if
                    end do
                end if
            end if

            selected_frame%selected_window = null_handle()
            call select_window(mru, status)
            if (.not. status%ok()) return
        end if

        call check_window_sides(status)
    end subroutine delete_window

    !> Delete specified WINDOW.
    !> WINDOW must be a valid window and defaults to the selected one.
    !>
    !> If the variable `ignore_window_parameters' is true, do not process
    !> any parameters of WINDOW.  An error condition occurs if WINDOW is
    !> either the only window on its frame or the last non-side window.
    !>
    !> If WINDOW is the selected window on its frame, choose some other
    !> window as that frame's selected window: the most recently used one
    !> (a window for which `is_window_no_other' returns true is chosen only
    !> if all windows on that frame have their `no_other_window' parameter
    !> set to true).
    !> Port of window.el:delete-window (the command).
    subroutine execute_delete_window(status)
        type(status_type), intent(out) :: status

        if (in_minibuffer(status)) return
        call delete_window(selected_frame%selected_window, status)
    end subroutine execute_delete_window

    module subroutine delete_other_windows(window, status, interactive)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: main, parent
        type(handle_type), allocatable :: all(:), others(:)
        type(status_type) :: ignored
        integer :: top, left, lines, columns, k
        logical :: keep_sides

        if (.not. is_window_valid(window)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected valid type(window_type)")
            return
        end if

        call check_window_sides(status)
        if (.not. status%ok()) return

        if (.not. ignore_window_parameters) then
            if (windows(window%index)%window_side /= WINDOW_SIDE_NONE) then
                call set_error(status, GENERIC_ERROR_CODE, "Cannot make side window the only window")
                return
            end if
        end if

        main = selected_frame%root_window

        if (.not. ignore_window_parameters) then
            call walk_window_tree(selected_frame%root_window, .false., all)

            if (any([(windows(all(k)%index)%no_delete_other_windows, k=1, size(all))])) then
                ! Keep the side windows (and use the main window) if each
                ! side window, and no other window, has a true
                ! `no_delete_other_windows' parameter.
                keep_sides = .true.

                do k = 1, size(all)
                    if ((windows(all(k)%index)%window_side /= WINDOW_SIDE_NONE) .neqv. &
                        windows(all(k)%index)%no_delete_other_windows) keep_sides = .false.
                end do

                if (keep_sides) then
                    main = window_main_window()
                else
                    ! Delete windows via `delete_window' because we found
                    ! either a deletable side window or a non-deletable
                    ! non-side-window.
                    others = window_list(status=ignored)

                    do k = 1, size(others)
                        if (.not. is_window_live(others(k))) cycle
                        if (same_window(others(k), window)) cycle
                        if (windows(others(k)%index)%no_delete_other_windows) cycle
                        call delete_window(others(k), ignored)
                    end do

                    return
                end if
            end if
        end if

        ! If WINDOW is the main window of its frame do nothing.
        if (same_window(window, main)) then
            ! Give a message to the user if this has been called as a
            ! command.
            if (present(interactive)) then
                if (interactive .and. .not. is_executing_keyboard_macro()) call message("No other windows to delete")
            end if

            return
        end if

        associate (r => windows(main%index))
            top = r%top_line
            left = r%left_column
            lines = r%total_lines
            columns = r%total_columns
        end associate

        ! Take WINDOW out of its combination, then free the rest.
        associate (w => windows(window%index))
            parent = w%parent

            if (is_window_valid(w%previous)) then
                windows(w%previous%index)%next = w%next
            else
                windows(parent%index)%child = w%next
            end if

            if (is_window_valid(w%next)) windows(w%next%index)%previous = w%previous
            w%parent = null_handle()
            w%next = null_handle()
            w%previous = null_handle()
        end associate

        if (same_window(main, selected_frame%root_window)) then
            ! The root window's normal sizes (window.c:replace_window).
            windows(window%index)%normal_lines = 1.0_real64
            windows(window%index)%normal_columns = 1.0_real64
            selected_frame%root_window = window
        else
            ! WINDOW takes MAIN's place, and its normal sizes.
            call replace_window(main, window)
        end if

        call unshow_leaves(main)
        call delete_window_tree(main)

        associate (r => windows(selected_frame%root_window%index))
            call resize_window_tree(window, top, left, lines, columns, r%left_column + r%total_columns)
        end associate

        if (is_window_live(window)) call select_window(window, status)
        if (.not. status%ok()) return
        call check_window_sides(status)
    end subroutine delete_other_windows

    !> Make WINDOW fill its frame.
    !> WINDOW must be a valid window and defaults to the selected one.
    !>
    !> If the variable `ignore_window_parameters' is true, do not pay
    !> attention to the parameters of WINDOW.  Otherwise, an error
    !> condition occurs if WINDOW is a side window.  Do not delete any
    !> window whose `no_delete_other_windows' parameter is true.
    !> Port of window.el:delete-other-windows (the command).
    subroutine execute_delete_other_windows(status)
        type(status_type), intent(out) :: status

        if (in_minibuffer(status)) return
        call delete_other_windows(selected_frame%selected_window, status, interactive=.true.)
    end subroutine execute_delete_other_windows

    module subroutine other_window(count, status, interactive)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type), allocatable :: leaves(:)
        integer(int64) :: left, old_count
        integer :: k, n, at, original, old_window

        if (in_minibuffer(status)) return
        call window_leaves(selected_frame%root_window, leaves)
        n = size(leaves)
        if (n == 0) return
        at = 1

        do k = 1, n
            if (same_window(leaves(k), selected_frame%selected_window)) at = k
        end do

        original = at
        left = count
        ! `next_window' and `previous_window' may return a window we are
        ! not allowed to select.  Hence we need an exit strategy in case
        ! all windows are non-selectable.
        old_window = 0
        old_count = 0

        do while (left /= 0)
            if (left > 0) then
                at = modulo(at, n) + 1
            else
                at = modulo(at - 2, n) + 1
            end if

            if (at == old_window) then
                ! Keep out of infinite loops.  When COUNT has not changed
                ! since we last looked at `window' we're probably in one.
                if (left == old_count) exit
            else if (is_window_no_other(leaves(at))) then
                if (old_window == 0) then
                    ! The first non-selectable window `next_window' got us:
                    ! remember it and the current value of COUNT.
                    old_window = at
                    old_count = left
                end if
            else if (left > 0) then
                left = left - 1
            else
                left = left + 1
            end if
        end do

        if (at == original .and. present(interactive)) then
            if (interactive .and. .not. is_executing_keyboard_macro()) call message("No other window to select")
        end if

        call select_window(leaves(at), status)
    end subroutine other_window

    !> Select another window in cyclic ordering of windows.
    !> COUNT specifies the number of windows to skip, starting with the
    !> selected window, before making the selection.  If COUNT is
    !> positive, skip COUNT windows forwards.  If COUNT is negative,
    !> skip -COUNT windows backwards.  COUNT zero means do not skip any
    !> window, so select the selected window.  In an interactive call,
    !> COUNT is the numeric prefix argument.
    !>
    !> Do not select a window for which `is_window_no_other' returns true.
    !> Port of window.el:other-window (the command; one frame).
    subroutine execute_other_window(status)
        type(status_type), intent(out) :: status

        call other_window(prefix_numeric_value(current_prefix_argument), status, interactive=.true.)
    end subroutine execute_other_window

    !> Select another window in the reverse cyclic ordering of windows.
    !> COUNT specifies the number of windows to skip, (by default) backward,
    !> starting with the selected window, before making the selection.  Like
    !> `other_window', but moves in the opposite direction.  Port of
    !> window.el:other-window-backward (the command; one frame).
    subroutine execute_other_window_backward(status)
        type(status_type), intent(out) :: status

        call other_window(-prefix_numeric_value(current_prefix_argument), status, interactive=.true.)
    end subroutine execute_other_window_backward

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
            call get_or_make_buffer("*scratch*", h, status)
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
            call get_or_make_buffer(name, buffer, status)
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
                call set_error(status, USER_ERROR_CODE, "Cannot switch buffers in minibuffer window")
                return
            end if

            ! (pop-to-buffer buffer norecord): NORECORD is the ACTION, a
            ! one that is .true. meaning `inhibit_same_window'.
            call pop_to_buffer(buffer, status, table=display_action_parameters_type(inhibit_same_window=norec))
            if (.not. status%ok()) return
        else if (windows(sw%index)%dedicated == DEDICATED_STRONG) then
            if (force) then
                call set_error(status, USER_ERROR_CODE, "Cannot switch buffers in a dedicated window")
                return
            end if

            ! (pop-to-buffer buffer norecord): NORECORD is the ACTION, a
            ! one that is .true. meaning `inhibit_same_window'.
            call pop_to_buffer(buffer, status, table=display_action_parameters_type(inhibit_same_window=norec))
            if (.not. status%ok()) return
        else
            ! `switch_to_buffer_preserve_window_point' is t.
            entry = .false.

            if (allocated(windows(sw%index)%previous_buffers)) then
                do k = 1, size(windows(sw%index)%previous_buffers)
                    if (same_handle(windows(sw%index)%previous_buffers(k)%buffer, buffer)) then
                        call marker_position(windows(sw%index)%previous_buffers(k)%start, start, status)
                        if (status%ok()) call marker_position(windows(sw%index)%previous_buffers(k)%point, point, status)
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
                call goto_character(point)
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
    recursive subroutine execute_switch_to_buffer(status)
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
                call set_error(status, USER_ERROR_CODE, "Cannot switch buffers in a dedicated window")
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
    end subroutine execute_switch_to_buffer

    !> Switch to buffer BUFFER_OR_NAME in another frame.
    !> BUFFER_OR_NAME may be a buffer, a string (a buffer name), or
    !> empty.  Return the buffer switched to.
    !>
    !> This uses the function `display_buffer' as a subroutine to display
    !> the buffer; see its documentation for additional customization
    !> information.  By default, if the buffer is already displayed (even
    !> in the current frame), that window is selected.  If the buffer
    !> isn't displayed in any frame, a new frame is popped up and the
    !> buffer is displayed there.
    !>
    !> If called interactively, read the buffer name using `read_buffer'.
    !> The variable `confirm_nonexistent_file_or_buffer' determines
    !> whether to request confirmation before creating a new buffer.
    !> See `read_buffer' for features related to input and completion of
    !> buffer names.
    !>
    !> If BUFFER_OR_NAME is a string and does not identify an existing
    !> buffer, create a new buffer with that name.  If BUFFER_OR_NAME is
    !> empty, switch to the buffer returned by `other_buffer'.
    !> Port of window.el:switch-to-buffer-other-frame (the command, with
    !> window.el:read-buffer-to-switch and
    !> window.el:window-normalize-buffer-to-switch-to).
    recursive subroutine execute_switch_to_buffer_other_frame(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: other, buffer
        integer(int64) :: bi

        other = other_buffer(current_buffer())
        call resolve_buffer(other, bi, status)
        if (.not. status%ok()) return
        call read_buffer("Switch to buffer in other frame: ", name, status, def=buffers(bi)%name, &
                         require_match=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, except=current_buffer())
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(bi)%name
        buffer = get_buffer(name)

        if (.not. is_buffer_live(buffer)) then
            call get_or_make_buffer(name, buffer, status)
            if (.not. status%ok()) return
        end if

        call switch_to_buffer_other_frame(buffer, status)
    end subroutine execute_switch_to_buffer_other_frame

    !> Display buffer BUFFER preferably in another frame.
    !> This function attempts to look for a window displaying BUFFER,
    !> on all the frames on the current terminal, skipping the selected
    !> window; if that fails, it pops up a new frame.
    !> This uses the function `display_buffer' as a subroutine; see its
    !> documentation for additional customization information.
    !> Port of window.el:display-buffer-other-frame (the command; callint.c
    !> `b': an existing buffer, the current one by default).
    recursive subroutine execute_display_buffer_other_frame(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: buffer, window
        integer(int64) :: bi

        call resolve_buffer(current_buffer(), bi, status)
        if (.not. status%ok()) return
        call read_buffer("Display buffer in other frame: ", name, status, def=buffers(bi)%name, &
                         require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(bi)%name
        buffer = get_buffer(name)
        call display_buffer_other_frame(buffer, window, status)
    end subroutine execute_display_buffer_other_frame

    !> Ask the user to confirm closing of a modified BUFFER.
    !> If the user confirms, optionally save BUFFER that is about to be
    !> closed.  `ok' is false if the user says no.  Port of
    !> simple.el:kill-buffer--possibly-save (read-multiple-choice with long
    !> answers).
    recursive subroutine close_buffer_possibly_save(buffer, ok, status)
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
    end subroutine close_buffer_possibly_save

    !> Close the buffer specified by BUFFER_OR_NAME.
    !> The argument may be a buffer or the name of an existing buffer.
    !> Argument omitted means close the current buffer.
    !>
    !> A modified buffer visiting a file is closed only after asking (and
    !> perhaps saving it).  Port of buffer.c:Fkill_buffer (the command:
    !> "bKill buffer: ", and kill-buffer--possibly-save).
    recursive subroutine execute_close_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: buffer
        integer(int64) :: bi
        logical :: modified, ok

        if (in_minibuffer(status)) return
        call read_buffer("Close buffer: ", name, status, def=buffers(current)%name, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(current)%name
        buffer = get_buffer(name)

        if (.not. is_buffer_live(buffer)) then
            call set_error(status, GENERIC_ERROR_CODE, "No such buffer "//name)
            return
        end if

        call resolve_buffer(buffer, bi, status)
        call is_buffer_modified(buffer, modified, status)

        if (modified .and. allocated(buffers(bi)%filename)) then
            call close_buffer_possibly_save(buffer, ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if

        call close_buffer(buffer, status)
    end subroutine execute_close_buffer

    module subroutine initialize_window_commands_submodule()
        commands%split_window_below%execute => execute_split_window_below
        commands%split_window_right%execute => execute_split_window_right
        commands%delete_window%execute => execute_delete_window
        commands%delete_other_windows%execute => execute_delete_other_windows
        commands%other_window%execute => execute_other_window
        commands%other_window_backward%execute => execute_other_window_backward
        commands%switch_to_buffer%execute => execute_switch_to_buffer
        commands%switch_to_buffer_other_frame%execute => execute_switch_to_buffer_other_frame
        commands%display_buffer_other_frame%execute => execute_display_buffer_other_frame
        commands%close_buffer%execute => execute_close_buffer
    end subroutine initialize_window_commands_submodule
end submodule window_commands_mod
