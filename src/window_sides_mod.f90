! window_sides_mod.f90 -- side windows.
! Ported from GNU Emacs 31.1 lisp/window.el (in part: walk-window-tree,
! window-with-parameter, window-no-other-p, the side windows section and
! display-buffer-in-side-window).
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

!> Part of window_mod: side windows.  A side window lives on one side
!> of the frame (its `window_side'), in a slot (`window_slot'); the
!> windows that are not side windows descend from the frame's main
!> window.  `display_buffer_in_side_window' makes and reuses them;
!> `split_window', `delete_window', `delete_other_windows' and
!> `other_window' (window_commands_mod) respect them.
submodule(window_mod) window_sides_mod
    use error_mod, only: set_error, GENERIC_ERROR_CODE
    use buffer_mod, only: is_buffer_live
    use frame_mod, only: selected_frame
    implicit none(type, external)

contains

    recursive module subroutine walk_window_tree(root, internal, list)
        type(handle_type), intent(in) :: root
        logical, intent(in) :: internal
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: sub(:)
        type(handle_type) :: child

        allocate (list(0))
        if (.not. is_window_valid(root)) return
        child = windows(root%index)%child
        if (internal .or. .not. is_window_valid(child)) list = [root]

        do while (is_window_valid(child))
            call walk_window_tree(child, internal, sub)
            list = [list, sub]
            child = windows(child%index)%next
        end do
    end subroutine walk_window_tree

    module function window_with_side(side, internal) result(window)
        integer, intent(in) :: side
        logical, intent(in) :: internal
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: k

        window = null_handle()
        call walk_window_tree(selected_frame%root_window, internal, list)

        do k = 1, size(list)
            associate (this_side => windows(list(k)%index)%window_side)
                if (this_side /= WINDOW_SIDE_NONE .and. (side == WINDOW_SIDE_NONE .or. side == this_side)) then
                    window = list(k)
                    return
                end if
            end associate
        end do
    end function window_with_side

    !> Whether `h' is a side window.  Not a port.
    logical function is_side(h)
        type(handle_type), intent(in) :: h

        is_side = .false.
        if (is_window_valid(h)) is_side = windows(h%index)%window_side /= WINDOW_SIDE_NONE
    end function is_side

    module function window_main_window() result(main)
        type(handle_type) :: main
        type(handle_type), allocatable :: list(:)
        integer :: k

        main = selected_frame%root_window
        ! Set main to the _last_ window found by `walk_window_tree' that is
        ! not a side window but has a side window as its sibling.
        call walk_window_tree(selected_frame%root_window, .true., list)

        do k = 1, size(list)
            if (is_side(list(k))) cycle
            if (is_side(windows(list(k)%index)%previous) .or. is_side(windows(list(k)%index)%next)) main = list(k)
        end do
    end function window_main_window

    module function is_window_no_other(window) result(p)
        type(handle_type), intent(in) :: window
        logical :: p

        p = .not. ignore_window_parameters .and. windows(window%index)%no_other_window
    end function is_window_no_other

    !> The name of SIDE (WINDOW_SIDE_*) as upstream prints the symbol.
    !> Not a port.
    function side_name(side) result(name)
        integer, intent(in) :: side
        character(len=:), allocatable :: name

        select case (side)
        case (WINDOW_SIDE_LEFT)
            name = "left"
        case (WINDOW_SIDE_TOP)
            name = "top"
        case (WINDOW_SIDE_RIGHT)
            name = "right"
        case (WINDOW_SIDE_BOTTOM)
            name = "bottom"
        case default
            name = ".false."
        end select
    end function side_name

    !> An error condition in STATUS if the frame's side windows are not
    !> set up as `check_window_sides' says.  Port of
    !> window.el:window--sides-check-failed (FRAME always has a main
    !> window: `window_main_window' falls back on the root window).
    subroutine window_sides_check_failed(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: side, k

        status = status_type()
        call walk_window_tree(selected_frame%root_window, .true., list)

        ! Now check the side windows.
        do side = WINDOW_SIDE_LEFT, WINDOW_SIDE_BOTTOM
            window = window_with_side(side, .true.)
            if (.not. is_window_valid(window)) cycle

            do k = 1, size(list)
                associate (this => list(k))
                    if (is_window_live(window)) then
                        ! If WINDOW is live there must be no other window on
                        ! this frame with the same `window_side' parameter.
                        if (windows(this%index)%window_side == side .and. .not. same_window(this, window)) then
                            call set_error(status, GENERIC_ERROR_CODE, "Window "//window_description(this)// &
                                           " has same side "//side_name(side)//" as window "// &
                                           window_description(window)//" but no common parent")
                            return
                        end if
                    else if (same_window(windows(this%index)%parent, window)) then
                        if (windows(this%index)%window_side /= side) then
                            call set_error(status, GENERIC_ERROR_CODE, "Window "//window_description(this)// &
                                           " has not same side "//side_name(side)//" as its parent "// &
                                           window_description(window))
                            return
                        end if
                    else if (windows(this%index)%window_side == side .and. .not. same_window(this, window)) then
                        call set_error(status, GENERIC_ERROR_CODE, "Window "//window_description(this)// &
                                       " has same side "//side_name(side)//" as major side window "// &
                                       window_description(window)//" but its parent is "// &
                                       window_description(windows(this%index)%parent))
                        return
                    end if
                end associate
            end do
        end do
    end subroutine window_sides_check_failed

    module subroutine check_window_sides(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (window_sides_inhibit_check) return
        if (.not. is_window_valid(window_with_side(WINDOW_SIDE_NONE, .true.))) return
        ! Upstream's check signals the error it finds, so its reset of the
        ! `window_side' parameters is never reached: STATUS returns the
        ! error.
        call window_sides_check_failed(status)
    end subroutine check_window_sides

    module function is_window_sides_reversed_on_frame() result(p)
        logical :: p
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: k

        if (window_sides_reversed%choice /= WINDOW_SIDES_REVERSED_BIDIRECTIONAL) then
            ! Reverse when `window_sides_reversed' is .true., do not
            ! reverse when it is .false.
            p = window_sides_reversed%value
            return
        end if

        ! Reverse when the frame's selected window shows a right-to-left
        ! buffer.
        p = .true.
        window = selected_frame%selected_window
        if (selected_frame%minibuffer_selected) window = selected_frame%minibuffer_window

        if (.not. is_side(window)) then
            if (windows(window%index)%mini) window = minibuffer_selected_window()

            if (is_window_live(window)) then
                if (buffers(windows(window%index)%buffer%index)%bidirectional_paragraph_direction == 2) return
            end if
        end if

        ! (Upstream next tries the frame's
        ! `window_sides_main_selected_window' parameter, which only the
        ! deferred reversing of existing side windows sets.)
        ! Reverse when all windows in the frame's main window show
        ! right-to-left buffers.
        call walk_window_tree(window_main_window(), .false., list)

        do k = 1, size(list)
            if (buffers(windows(list(k)%index)%buffer%index)%bidirectional_paragraph_direction == 1) then
                p = .false.
                return
            end if
        end do
    end function is_window_sides_reversed_on_frame

    !> Return window to split for making a major side window.
    !> SIDE must be one of WINDOW_SIDE_LEFT, WINDOW_SIDE_TOP,
    !> WINDOW_SIDE_RIGHT or WINDOW_SIDE_BOTTOM.
    !>
    !> This is an auxiliary function of `make_major_side_window'
    !> and must not be called when a window on SIDE exists already.
    !> Port of window.el:window--make-major-side-window-next-to.
    function make_major_side_window_next_to(side) result(window)
        integer, intent(in) :: side
        type(handle_type) :: window
        type(handle_type) :: root, other

        root = selected_frame%root_window
        window = root

        ! (1) If a window on the opposite side exists, return that window's
        !     sibling.
        ! (2) If the new window shall span the entire side, return the
        !     frame's root window.
        ! (3) If a window on an orthogonal side exists, return that
        !     window's sibling.
        ! (4) Otherwise return the frame's root window.
        select case (side)
        case (WINDOW_SIDE_LEFT, WINDOW_SIDE_TOP)
            if (side == WINDOW_SIDE_LEFT) then
                other = window_with_side(WINDOW_SIDE_RIGHT, .true.)
            else
                other = window_with_side(WINDOW_SIDE_BOTTOM, .true.)
            end if

            if (is_window_valid(other)) then
                window = windows(other%index)%previous
                return
            end if
        case (WINDOW_SIDE_RIGHT, WINDOW_SIDE_BOTTOM)
            if (side == WINDOW_SIDE_RIGHT) then
                other = window_with_side(WINDOW_SIDE_LEFT, .true.)
            else
                other = window_with_side(WINDOW_SIDE_TOP, .true.)
            end if

            if (is_window_valid(other)) then
                window = windows(other%index)%next
                return
            end if
        case default
            continue
        end select

        if (side == WINDOW_SIDE_LEFT .or. side == WINDOW_SIDE_RIGHT) then
            if (window_sides_vertical) return
            other = window_with_side(WINDOW_SIDE_TOP, .true.)

            if (is_window_valid(other)) then
                window = windows(other%index)%next
                return
            end if

            other = window_with_side(WINDOW_SIDE_BOTTOM, .true.)
            if (is_window_valid(other)) window = windows(other%index)%previous
        else
            if (.not. window_sides_vertical) return
            other = window_with_side(WINDOW_SIDE_LEFT, .true.)

            if (is_window_valid(other)) then
                window = windows(other%index)%next
                return
            end if

            other = window_with_side(WINDOW_SIDE_RIGHT, .true.)
            if (is_window_valid(other)) window = windows(other%index)%previous
        end if
    end function make_major_side_window_next_to

    !> Display BUFFER in a new major side window on the selected frame.
    !> SIDE must be one of WINDOW_SIDE_LEFT, WINDOW_SIDE_TOP,
    !> WINDOW_SIDE_RIGHT or WINDOW_SIDE_BOTTOM.  SLOT specifies the slot
    !> to use.  TABLE is a buffer display action table as passed to
    !> `display_buffer_in_side_window'.  Return the new window in WINDOW,
    !> the null handle if its creation failed.
    !>
    !> This is an auxiliary function of `display_buffer_in_side_window'
    !> and may be called only if no window on SIDE exists yet.  Port of
    !> window.el:window--make-major-side-window (no window is merged into
    !> another, so the main window needs no combination limit).
    subroutine make_major_side_window(buffer, side, slot, table, window, status)
        type(handle_type), intent(in) :: buffer
        integer, intent(in) :: side, slot
        type(display_action_parameters_type), intent(in) :: table
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        type(display_action_parameters_type) :: entries
        type(handle_type) :: next_to
        logical :: left_or_right, saved_ignore, saved_inhibit
        type(status_type) :: ignored

        status = status_type()
        window = null_handle()
        left_or_right = side == WINDOW_SIDE_LEFT .or. side == WINDOW_SIDE_RIGHT
        saved_inhibit = window_sides_inhibit_check
        window_sides_inhibit_check = .true.
        next_to = make_major_side_window_next_to(side)
        ! Splitting with `side_resize' takes the space for the new window
        ! from the selected frame's main window and makes no new parent
        ! window unless needed.
        saved_ignore = ignore_window_parameters
        ignore_window_parameters = .true.
        call split_window(next_to, 0, .false., left_or_right, window, ignored, &
                          before=side == WINDOW_SIDE_LEFT .or. side == WINDOW_SIDE_TOP, side_resize=.true.)
        ignore_window_parameters = saved_ignore
        window_sides_inhibit_check = saved_inhibit
        if (.not. ignored%ok()) window = null_handle()
        if (.not. is_window_live(window)) return
        entries = table

        if (.not. entries%has_dedicated) then
            entries%has_dedicated = .true.
            entries%dedicated = display_buffer_mark_dedicated
            if (entries%dedicated == DEDICATED_NONE) entries%dedicated = DEDICATED_SIDE
        end if

        ! Initialize `window_side' parameter of new window to SIDE and
        ! install its `window_slot' parameter.
        windows(window%index)%window_side = side
        windows(window%index)%window_slot = slot

        ! Auto-adjust height/width of new window unless a size has been
        ! explicitly requested: by default use a fourth of the size of
        ! the frame's root window.
        associate (root => windows(selected_frame%root_window%index))
            if (left_or_right) then
                if (entries%window_width%columns == 0 .and. entries%window_width%fraction == 0.0_real64) then
                    entries%window_width%columns = root%total_columns/4
                end if
            else if (entries%window_height%lines == 0 .and. entries%window_height%fraction == 0.0_real64 .and. &
                     .not. associated(entries%window_height%procedure)) then
                entries%window_height%lines = root%total_lines/4
            end if
        end associate

        if (is_buffer_live(buffer)) buffers(buffer%index)%window_sides_shown = .true.
        ! Install BUFFER in new window and return WINDOW.
        window = window_display_buffer(buffer, window, QR_WINDOW, status, entries)
    end subroutine make_major_side_window

    module subroutine display_buffer_in_side_window(buffer, table, window, status)
        type(handle_type), intent(in) :: buffer
        type(display_action_parameters_type), intent(in) :: table
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        type(display_action_parameters_type) :: entries
        type(handle_type) :: major, first, next, this_window, previous_window, next_window, best_window, new
        type(handle_type), allocatable :: list(:)
        type(status_type) :: ignored
        character(len=20) :: number
        integer :: side, slot, slots, maximum_slots, this_slot, best_slot, absolute_slot, k, n
        logical :: left_or_right, reversed, has_best, saved_inhibit

        status = status_type()
        window = null_handle()
        side = table%side
        slot = table%slot

        if (side < WINDOW_SIDE_LEFT .or. side > WINDOW_SIDE_BOTTOM) then
            write (number, "(i0)") side
            call set_error(status, GENERIC_ERROR_CODE, "Invalid side "//trim(number)//" specified")
            return
        end if

        left_or_right = side == WINDOW_SIDE_LEFT .or. side == WINDOW_SIDE_RIGHT
        ! `major' is the major window on SIDE, `list' the live windows on
        ! SIDE.
        major = window_with_side(side, .true.)
        reversed = is_window_sides_reversed_on_frame()
        allocate (list(0))
        slots = 0

        if (is_window_live(major)) then
            list = [major]
        else if (is_window_valid(major)) then
            first = windows(major%index)%child
            next = windows(first%index)%next
            reversed = windows(first%index)%window_slot > windows(next%index)%window_slot
            next = first

            do while (is_window_valid(next))
                list = [list, next]
                next = windows(next%index)%next
            end do

            if (reversed) list = list(size(list):1:-1)
        end if

        if (is_window_valid(major)) slots = max(1, count_window_children(major))

        select case (side)
        case (WINDOW_SIDE_LEFT)
            maximum_slots = window_sides_slots%left
        case (WINDOW_SIDE_TOP)
            maximum_slots = window_sides_slots%top
        case (WINDOW_SIDE_RIGHT)
            maximum_slots = window_sides_slots%right
        case default
            maximum_slots = window_sides_slots%bottom
        end select

        saved_inhibit = window_sides_inhibit_check
        window_sides_inhibit_check = .true.
        entries = table

        if (.not. entries%has_dedicated) then
            entries%has_dedicated = .true.
            entries%dedicated = display_buffer_mark_dedicated
            if (entries%dedicated == DEDICATED_NONE) entries%dedicated = DEDICATED_SIDE
        end if

        if (maximum_slots <= 0) then
            ! No side-slots available on this side.  Don't raise an error,
            ! just return the null handle.
            window_sides_inhibit_check = saved_inhibit
            return
        end if

        if (size(list) == 0) then
            ! No major side window exists on this side, make one.
            call make_major_side_window(buffer, side, slot, entries, window, status)
            window_sides_inhibit_check = saved_inhibit
            return
        end if

        ! Scan windows on SIDE.
        this_window = null_handle()
        previous_window = null_handle()
        next_window = null_handle()
        best_window = null_handle()
        has_best = .false.
        best_slot = 0
        n = size(list)

        do k = 1, n
            this_slot = windows(list(k)%index)%window_slot

            if (this_slot == slot) then
                ! A window with a matching slot has been found.
                this_window = list(k)
                exit
            end if

            ! Check if this window has a better slot value wrt the slot of
            ! the window we want.
            if ((this_slot > 0 .and. slot > 0) .or. (this_slot < 0 .and. slot < 0)) then
                absolute_slot = abs(slot - this_slot)
            else
                absolute_slot = abs(slot) + abs(this_slot)
            end if

            if (.not. has_best .or. best_slot > absolute_slot) then
                best_window = list(k)
                best_slot = absolute_slot
                has_best = .true.
            end if

            if (reversed) then
                if (this_slot <= slot) then
                    next_window = list(k)
                else if (previous_window%index == 0) then
                    previous_window = list(k)
                end if
            else if (this_slot <= slot) then
                previous_window = list(k)
            else if (next_window%index == 0) then
                next_window = list(k)
            end if
        end do

        ! `this_window' is the first window with the same SLOT.
        ! `previous_window' is the window with the largest slot < SLOT.  A new
        ! window will be created after it.
        ! `next_window' is the window with the smallest slot > SLOT.  A new
        ! window will be created before it.
        ! `best_window' is the window with the smallest absolute difference
        ! of its slot and SLOT.
        if (is_window_live(this_window)) then
            ! Reuse `this_window'.
            if (is_buffer_live(buffer)) buffers(buffer%index)%window_sides_shown = .true.
            window = window_display_buffer(buffer, this_window, QR_OTHER, status, entries)

            if (is_window_live(window) .or. .not. status%ok()) then
                window_sides_inhibit_check = saved_inhibit
                return
            end if
        end if

        if (slots < maximum_slots) then
            new = null_handle()

            if (is_window_live(next_window)) then
                ! Make new window before `next_window'.
                call split_window(next_window, 0, .false.,.not. left_or_right, new, ignored, before=.true., &
                                  side_resize=.true.)
                if (.not. ignored%ok()) new = null_handle()
            end if

            if (.not. is_window_live(new) .and. is_window_live(previous_window)) then
                ! Make new window after `previous_window'.
                call split_window(previous_window, 0, .false.,.not. left_or_right, new, ignored, side_resize=.true.)
                if (.not. ignored%ok()) new = null_handle()
            end if

            if (is_window_live(new)) then
                windows(new%index)%window_slot = slot
                if (is_buffer_live(buffer)) buffers(buffer%index)%window_sides_shown = .true.
                window = window_display_buffer(buffer, new, QR_WINDOW, status, entries)

                if (is_window_live(window) .or. .not. status%ok()) then
                    window_sides_inhibit_check = saved_inhibit
                    return
                end if
            end if
        end if

        if (is_window_live(best_window)) then
            ! Reuse `best_window', giving it the new slot value.
            windows(best_window%index)%window_slot = slot
            if (is_buffer_live(buffer)) buffers(buffer%index)%window_sides_shown = .true.
            window = window_display_buffer(buffer, best_window, QR_OTHER, status, entries)
        end if

        window_sides_inhibit_check = saved_inhibit
    end subroutine display_buffer_in_side_window

    !> Return number of WINDOW's child windows.  Port of
    !> window.el:window-child-count.
    integer function count_window_children(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: child

        count_window_children = 0
        child = windows(h%index)%child

        do while (is_window_valid(child))
            count_window_children = count_window_children + 1
            child = windows(child%index)%next
        end do
    end function count_window_children
end submodule window_sides_mod
