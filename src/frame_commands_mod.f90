! frame_commands_mod.f90 -- making, selecting and deleting terminal frames.
! Ported from GNU Emacs 31.1 src/frame.c and lisp/frame.el.
!
! Copyright (C) 1993-1995, 1997, 1999-2026 Free Software Foundation, Inc.
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

!> Several frames on one terminal.  The terminal shows one frame at a
!> time, the selected one (its top frame); selecting another frame hands
!> it the terminal's state (frame_type's `tty') and redraws the screen.
!> Each frame has its own windows, its own minibuffer window and its
!> own buffer lists.  A submodule of frame_mod, so that frame_mod stays
!> below the display modules it needs (display_update_mod, display_engine_mod,
!> face_realization_mod).  Also frame.el's Window Divider mode, a minor mode
!> (easy_minor_mode_mod, which depends on frame_mod, hence the submodule): it
!> sets the divider widths of every frame and of new ones
!> (`default_frame'); a terminal frame stores them as parameters but draws
!> no dividers, as upstream on a text terminal.  What is left out is
!> listed in docs/status/frame_mod.md.
submodule(frame_mod) frame_commands_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list
    use buffer_mod, only: buffers, current_buffer, resolve_buffer, exchange_frame_buffers, is_buffer_live
    use window_mod, only: windows, window_leaves, select_window, set_window_buffer_internal, other_buffer, &
                          delete_window_object, unshow_buffer, set_window_point, window_point, is_window_live
    use display_update_mod, only: adjust_frame_glyphs, adjust_frame_size
    use display_engine_mod, only: initialize_frame_windows
    use face_realization_mod, only: recompute_basic_faces
    use minibuffer_reading_mod, only: minibuf_level, nth_minibuffer, read_string, completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Making frames
    ! ------------------------------------------------------------------

    !> Return a newly created frame displaying the current buffer.  On a
    !> text terminal the frame is made on the selected frame's terminal
    !> and is as large as the terminal; it is not selected.  Port of
    !> frame.el:make-frame (PARAMETERS absent; tty-create-frame-with-faces
    !> as `frame_creation_function'), frame.c:Fmake_terminal_frame,
    !> frame.c:make_terminal_frame and frame.c:make_frame (a frame with
    !> its own minibuffer window).
    module subroutine make_frame(frame, status)
        type(handle_type), intent(out) :: frame
        type(status_type), intent(out) :: status
        type(frame_type), pointer :: f, top
        type(handle_type) :: buffer
        integer(int64) :: bi

        call ensure_frame_windows()
        status = status_type()
        allocate (f)
        ! The new frame is on the selected frame's terminal, whose state
        ! its top frame holds.
        top => get_frame(top_frame)
        if (.not. associated(top)) top => selected_frame
        f%tty = top%tty
        f%column_width = selected_frame%column_width
        f%line_height = selected_frame%line_height
        f%smallest_character_width = selected_frame%smallest_character_width
        f%smallest_font_height = selected_frame%smallest_font_height
        call register_frame(f)
        f%name = get_next_frame_name()
        f%visible = .true.
        ! frame.c:make_frame: choose a buffer for the frame's root window;
        ! if the current buffer is hidden, try to find another one.
        buffer = current_buffer()
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return

        if (len(buffers(bi)%name) > 0) then
            if (buffers(bi)%name(1:1) == " ") buffer = other_buffer(buffer)
        end if

        ! The `tab-bar-lines' parameter of `default_frame'.
        f%tab_bar_lines = default_frame%tab_bar_lines
        call adjust_frame_glyphs(f)
        call initialize_frame_windows(f)
        call set_window_buffer_internal(f%root_window, buffer, .false.)
        f%buffers = [buffer]
        allocate (f%buried_buffers(0))
        ! tty-create-frame-with-faces: the faces of the terminal.
        if (allocated(selected_frame%faces)) f%faces = selected_frame%faces
        f%needs_redraw = .true.
        frame = f%handle
    end subroutine make_frame

    ! ------------------------------------------------------------------
    ! Selecting frames
    ! ------------------------------------------------------------------

    !> Select FRAME.
    !> Subsequent editing commands apply to its selected window.
    !> Optional argument NORECORD means to neither change the order of
    !> recently selected windows nor the buffer list.
    !>
    !> The selection of FRAME lasts until the next time the user does
    !> something to select a different frame, or until the next time
    !> this function is called.  If you are using a window system, the
    !> previously selected frame may be restored as the selected frame
    !> when returning to the command loop, because it still may have
    !> the window system's input focus.  On a text terminal, the next
    !> redisplay will display FRAME.
    !>
    !> This function returns FRAME, or a null handle if FRAME has been
    !> deleted.  Port of frame.c:Fselect_frame and frame.c:do_switch_frame
    !> (a terminal frame; FOR_DELETION when the selected frame is being
    !> deleted).
    recursive module subroutine select_frame(frame, status, norecord, for_deletion)
        type(handle_type), intent(in) :: frame
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord, for_deletion
        type(frame_type), pointer :: f, sf, top
        integer(int64) :: bi
        type(status_type) :: st
        logical :: select_mini_window, deleting

        status = status_type()
        call ensure_frame_windows()
        deleting = .false.
        if (present(for_deletion)) deleting = for_deletion
        f => get_frame(frame)
        sf => selected_frame
        ! Silently ignore dead frames.
        if (.not. associated(f)) return
        if (associated(f, sf)) return

        ! The window left keeps its buffer's point (what select_window does
        ! for the window that was selected).
        if (is_window_live(sf%selected_window)) then
            call resolve_buffer(windows(sf%selected_window%index)%buffer, bi, st)
            if (st%ok()) call set_window_point(windows(sf%selected_window%index), buffers(bi)%point, st)
        end if

        ! FRAME becomes its terminal's top frame: it takes the terminal's
        ! state, and its windows and matrices the terminal's size (all
        ! frames of a terminal have its size), and it is redrawn.
        top => get_frame(top_frame)
        if (.not. associated(top)) top => sf
        if (.not. associated(top, f)) f%tty = top%tty
        top_frame = f%handle
        call adjust_frame_size(f, frame_columns(f)*f%column_width, frame_lines(f)*f%line_height)
        call adjust_frame_glyphs(f)
        f%needs_redraw = .true.
        f%visible = .true.
        select_mini_window = sf%minibuffer_selected
        call move_minibuffers_onto_frame(sf, f, deleting)
        ! Each frame has its own buffer lists.
        call exchange_frame_buffers(f%buffers, f%buried_buffers)
        call move_alloc(f%buffers, sf%buffers)
        call move_alloc(f%buried_buffers, sf%buried_buffers)
        selected_frame => f
        ! The minibuffer window is selected again if it has an active
        ! minibuffer.
        f%minibuffer_selected = select_mini_window .and. minibuf_level > 0
        ! Faces may have changed while FRAME was not selected.
        if (allocated(f%faces)) call recompute_basic_faces(f)
        call select_window(f%selected_window, status, norecord)
    end subroutine select_frame

    !> Move the active minibuffer of frame SF, if any, onto frame F, the
    !> frame being selected: F's minibuffer window shows it and SF's the
    !> inactive minibuffer.  FOR_DELETION means SF is being deleted.
    !> Port of minibuf.c:move_minibuffers_onto_frame
    !> (`minibuffer_follows_selected_frame' .true., its default).
    subroutine move_minibuffers_onto_frame(sf, f, for_deletion)
        type(frame_type), intent(inout) :: sf, f
        logical, intent(in) :: for_deletion
        type(handle_type) :: inactive

        if (minibuf_level == 0) return
        if (.not. is_window_live(sf%minibuffer_window)) return
        if (.not. is_window_live(f%minibuffer_window)) return
        windows(f%minibuffer_window%index)%buffer = windows(sf%minibuffer_window%index)%buffer
        if (for_deletion) return
        inactive = nth_minibuffer(0)
        if (is_buffer_live(inactive)) windows(sf%minibuffer_window%index)%buffer = inactive
        sf%minibuffer_selected = .false.
    end subroutine move_minibuffers_onto_frame

    !> Make FRAME the selected frame for what follows, without showing it
    !> or recording it: the frame selected is stored in SAVED, which
    !> `select_frame_end' selects again.  Port of subr.el:with-selected-frame
    !> (the terminal keeps showing the frame it shows).
    module subroutine select_frame_begin(frame, saved)
        type(handle_type), intent(in) :: frame
        type(handle_type), intent(out) :: saved
        type(frame_type), pointer :: f

        call ensure_frame_windows()
        saved = selected_frame%handle
        f => get_frame(frame)
        if (.not. associated(f)) return
        if (associated(f, selected_frame)) return
        call exchange_frame_buffers(f%buffers, f%buried_buffers)
        call move_alloc(f%buffers, selected_frame%buffers)
        call move_alloc(f%buried_buffers, selected_frame%buried_buffers)
        selected_frame => f
    end subroutine select_frame_begin

    !> Select again the frame SAVED that `select_frame_begin' stored.  If
    !> the terminal shows another frame meanwhile, SAVED is selected as
    !> `select_frame' does.  Port of subr.el:with-selected-frame (the
    !> restoring of the frame selected before).
    recursive module subroutine select_frame_end(saved)
        type(handle_type), intent(in) :: saved
        type(frame_type), pointer :: f
        type(status_type) :: st

        f => get_frame(saved)
        if (.not. associated(f)) return
        if (associated(f, selected_frame)) return

        if (.not. same_handle(saved, top_frame)) then
            call select_frame(saved, st, norecord=.true.)
            return
        end if

        call exchange_frame_buffers(f%buffers, f%buried_buffers)
        call move_alloc(f%buffers, selected_frame%buffers)
        call move_alloc(f%buried_buffers, selected_frame%buried_buffers)
        selected_frame => f
    end subroutine select_frame_end

    ! ------------------------------------------------------------------
    ! Deleting frames
    ! ------------------------------------------------------------------

    !> Whether there is a frame other than F that can stay when F is
    !> deleted: on a terminal every other frame is visible.  Port of
    !> frame.c:other_frames (INVISIBLE false; no tooltip, child or
    !> `delete-before' frames).
    logical function has_other_frames(f)
        type(frame_type), intent(in) :: f

        has_other_frames = any(frame_order%index /= f%handle%index)
    end function has_other_frames

    !> The highest use time of any window on frame F.  Port of
    !> frame.el:frame-use-time.
    integer(int64) function get_frame_use_time(f) result(time)
        type(frame_type), intent(in) :: f
        type(handle_type), allocatable :: leaves(:)
        integer :: k

        time = 0
        if (.not. is_window_valid(f%root_window)) return
        call window_leaves(f%root_window, leaves)

        do k = 1, size(leaves)
            time = max(time, windows(leaves(k)%index)%use_time)
        end do
    end function get_frame_use_time

    !> The most recently used frame other than EXCLUDE (by
    !> `get_frame_use_time'; the first in the frame list among equals), or a
    !> null handle if there is none.  Port of frame.el:get-mru-frame and
    !> frame.el:get-mru-frames (ALL-FRAMES `visible' on a terminal: all
    !> frames).
    function get_mru_frame(exclude) result(frame)
        type(handle_type), intent(in) :: exclude
        type(handle_type) :: frame
        integer(int64) :: best, time
        integer :: k

        frame = null_handle()
        best = -1

        do k = 1, size(frame_order)
            if (frame_order(k)%index == exclude%index) cycle
            time = get_frame_use_time(frame_slots(frame_order(k)%index)%frame)

            if (time > best) then
                best = time
                frame = frame_order(k)
            end if
        end do
    end function get_mru_frame

    !> Delete FRAME, eliminating it from use.
    !> FRAME must be a live frame and defaults to the selected one.
    !>
    !> When `undelete_frame_mode' is enabled, the 16 most recently deleted
    !> frames can be undeleted with `undelete_frame', which see.
    !>
    !> A frame may not be deleted if its minibuffer serves as surrogate
    !> minibuffer for another frame.  Normally, you may not delete a frame
    !> if all other frames are invisible, but if the second optional
    !> argument FORCE is true, you may do so.
    !>
    !> This function runs `delete_frame_functions' before actually
    !> deleting the frame, unless the frame is a tooltip.
    !> The functions are run with one argument, the frame to be deleted.
    !> Port of frame.c:Fdelete_frame and frame.c:delete_frame (a terminal
    !> frame; `delete_frame_choose_selected' `mru', its default).
    recursive module subroutine delete_frame(status, frame, force)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: frame
        logical, intent(in), optional :: force
        type(frame_type), pointer :: f
        type(frame_type) :: empty
        type(handle_type) :: frame1
        logical :: forced

        status = status_type()
        call ensure_frame_windows()
        forced = .false.
        if (present(force)) forced = force
        f => selected_frame
        if (present(frame)) f => get_frame(frame)
        if (.not. associated(f)) return

        if (.not. has_other_frames(f)) then
            if (forced) then
                call set_error(status, GENERIC_ERROR_CODE, "Attempt to delete the only frame")
            else
                call set_error(status, GENERIC_ERROR_CODE, "Attempt to delete the sole visible or iconified frame")
            end if

            return
        end if

        if (associated(f, selected_frame)) then
            ! Select the most recently used frame on the same terminal.
            frame1 = get_mru_frame(f%handle)
            if (frame1%index == 0) frame1 = frame_order(findloc(frame_order%index /= f%handle%index, .true., dim=1))
            call select_frame(frame1, status, for_deletion=.true.)
            if (.not. status%ok()) return
        else
            call move_minibuffers_onto_frame(f, selected_frame, .true.)

            ! The terminal shows the selected frame instead.
            if (same_handle(f%handle, top_frame)) then
                selected_frame%tty = f%tty
                selected_frame%needs_redraw = .true.
                top_frame = selected_frame%handle
            end if
        end if

        call delete_frame_tabs(f)
        call delete_frame_windows(f)
        call unregister_frame(f)

        if (associated(f, initial_frame)) then
            initial_frame = empty
        else
            deallocate (f)
        end if
    end subroutine delete_frame

    !> Free the tabs and the tab history of frame F, which is being
    !> deleted.  Not a port (upstream relies on GC).
    subroutine delete_frame_tabs(f)
        type(frame_type), intent(inout) :: f
        integer :: k

        if (allocated(f%parameters%tabs)) then
            do k = 1, size(f%parameters%tabs)
                call f%parameters%tabs(k)%destroy()
            end do

            deallocate (f%parameters%tabs)
        end if

        if (allocated(f%tab_history_back)) then
            do k = 1, size(f%tab_history_back)
                call f%tab_history_back(k)%destroy()
            end do

            deallocate (f%tab_history_back)
        end if

        if (allocated(f%tab_history_forward)) then
            do k = 1, size(f%tab_history_forward)
                call f%tab_history_forward(k)%destroy()
            end do

            deallocate (f%tab_history_forward)
        end if
    end subroutine delete_frame_tabs

    module subroutine set_tab_bar_lines(f, n)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: n

        ! (A window-system frame's tab bar, frame.c:gui_set_tab_bar_lines, waits
        ! for the GUI.)
        if (associated(f%rif)) return
        if (n == f%tab_bar_lines) return
        f%tab_bar_lines = n
        ! change_frame_size with the frame's size: the windows move below
        ! the tab bar, or up.
        call adjust_frame_size(f, frame_columns(f)*f%column_width, frame_lines(f)*f%line_height)
    end subroutine set_tab_bar_lines

    !> Delete the windows of frame F, which is being deleted: each window
    !> stops showing its buffer.  Port of frame.c:delete_frame (its
    !> `free_glyphs' and `delete_all_child_windows' part).
    subroutine delete_frame_windows(f)
        type(frame_type), intent(inout) :: f

        if (is_window_valid(f%root_window)) call delete_window_tree(f%root_window)
        if (is_window_valid(f%minibuffer_window)) call delete_window_object(f%minibuffer_window)
        f%root_window = null_handle()
        f%selected_window = null_handle()
        f%minibuffer_window = null_handle()
    end subroutine delete_frame_windows

    !> Delete window H and the windows below it.  Port of
    !> window.c:delete_all_child_windows (the windows are not kept for a
    !> revival).
    recursive subroutine delete_window_tree(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: child, next

        child = windows(h%index)%child

        do while (is_window_valid(child))
            next = windows(child%index)%next
            call delete_window_tree(child)
            child = next
        end do

        if (is_window_live(h)) call unshow_buffer(h)
        call delete_window_object(h)
    end subroutine delete_window_tree

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Make a new frame, on the same terminal as the selected frame.
    !> If the terminal is a text-only terminal, this also selects the
    !> new frame.  Port of frame.el:make-frame-command.
    subroutine execute_make_frame_command(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: frame

        call make_frame(frame, status)
        if (.not. status%ok()) return
        call select_frame(frame, status)
    end subroutine execute_make_frame_command

    !> Select the ARGth visible frame on current display, and raise it.
    !> All frames are arranged in a cyclic order.  This command selects
    !> the frame ARGUMENT steps away from the selected frame in that order.  A
    !> negative ARGUMENT moves in the opposite order.  It does not select a
    !> minibuffer-only frame.  Port of frame.el:other-frame.
    recursive module subroutine other_frame(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type) :: frame
        integer(int64) :: k

        call ensure_frame_windows()
        frame = selected_frame%handle

        do k = 1, argument
            frame = next_frame(frame)
        end do

        do k = 1, -argument
            frame = previous_frame(frame)
        end do

        call select_frame(frame, status)
    end subroutine other_frame

    !> The command form of `other_frame'.  Port of frame.el:other-frame
    !> (the command).
    subroutine execute_other_frame(status)
        type(status_type), intent(out) :: status

        call other_frame(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_other_frame

    !> Delete FRAME, eliminating it from use.
    !> FRAME must be a live frame and defaults to the selected one.
    !> Normally, you may not delete a frame if all other frames are
    !> invisible, but if the second optional argument FORCE is true,
    !> you may do so.  Port of frame.c:Fdelete_frame (the command).
    subroutine execute_delete_frame(status)
        type(status_type), intent(out) :: status

        call delete_frame(status)
    end subroutine execute_delete_frame

    !> Delete all frames on FRAME's terminal, except FRAME.
    !> If FRAME uses another frame's minibuffer, the minibuffer frame is
    !> left untouched.  Do not delete any of FRAME's child frames.  If
    !> FRAME is a child frame, delete its siblings only.  FRAME must be a
    !> live frame and defaults to the selected one.
    !> If the prefix arg ICONIFY is true, just iconify the frames rather
    !> than deleting them.  Port of frame.el:delete-other-frames (a
    !> terminal frame cannot be iconified: frame.c:Ficonify_frame does
    !> nothing).
    recursive module subroutine delete_other_frames(status, frame, iconify)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: frame
        logical, intent(in), optional :: iconify
        type(handle_type), allocatable :: frames(:)
        type(handle_type) :: keep
        integer :: k

        status = status_type()
        call ensure_frame_windows()
        keep = selected_frame%handle
        if (present(frame)) keep = frame

        if (.not. is_frame_live(keep)) then
            call set_error(status, GENERIC_ERROR_CODE, "Not a live frame")
            return
        end if

        if (present(iconify)) then
            if (iconify) return
        end if

        frames = frame_list()

        do k = 1, size(frames)
            if (same_handle(frames(k), keep)) cycle
            if (.not. is_frame_live(frames(k))) cycle
            call delete_frame(status, frames(k))
            if (.not. status%ok()) return
        end do
    end subroutine delete_other_frames

    !> The command form of `delete_other_frames'.  Port of
    !> frame.el:delete-other-frames (the command).
    subroutine execute_delete_other_frames(status)
        type(status_type), intent(out) :: status

        call delete_other_frames(status, iconify=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_delete_other_frames

    !> Select the frame whose name is NAME and raise it.
    !> Frames on the current terminal are checked first.
    !> Raise the frame and give it input focus.  On a text terminal, the
    !> frame will occupy the entire terminal screen after the next
    !> redisplay.
    !> If there is no frame by that name, an `error' error condition
    !> occurs.  Port of frame.el:select-frame-by-name.
    recursive module subroutine select_frame_by_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        type(handle_type) :: frame

        frame = find_frame(name)

        if (frame%index == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "There is no frame named `"//name//"'")
            return
        end if

        call select_frame(frame, status)
    end subroutine select_frame_by_name

    !> The command form of `select_frame_by_name': read a frame name with
    !> completion, the selected frame's by default.  Port of
    !> frame.el:select-frame-by-name (the command) with
    !> frame.el:make-frame-names-alist and frame.el:frame-list-1.
    subroutine execute_select_frame_by_name(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: input, default
        type(handle_type) :: frame
        integer :: k, n

        call ensure_frame_windows()
        ! The frames starting with the selected one.
        n = size(frame_order)
        allocate (names(n))
        frame = selected_frame%handle

        do k = 1, n
            names(k) = value_type(text=frame_slots(frame%index)%frame%name)
            frame = frame_order(modulo(findloc(frame_order%index, frame%index, dim=1), n) + 1)
        end do

        default = selected_frame%name
        call completing_read("Select Frame (default "//default//"): ", completion_list_table(make_list(names)), input, &
                             status, require_match=REQUIRE_MATCH_YES, history="frame_name_history")
        if (.not. status%ok()) return
        if (len(input) == 0) input = default
        call select_frame_by_name(input, status)
    end subroutine execute_select_frame_by_name

    !> Set the name of the selected frame to NAME.
    !> When called interactively, prompt for the name of the frame.
    !> On text terminals, the frame name is displayed on the mode line.
    !> On graphical displays, it is displayed on the frame's title bar.
    !> Port of frame.el:set-frame-name (with frame.c:Fmodify_frame_parameters
    !> for the `name' parameter on a terminal).
    module subroutine set_frame_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status

        call ensure_frame_windows()
        call set_term_frame_name(selected_frame, status, name)
    end subroutine set_frame_name

    !> The command form of `set_frame_name'.  Port of
    !> frame.el:set-frame-name (the command).
    subroutine execute_set_frame_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, default

        call ensure_frame_windows()
        default = selected_frame%name
        call read_string("Frame name (default "//default//"): ", name, status, default=default)
        if (.not. status%ok()) return
        call set_frame_name(name, status)
    end subroutine execute_set_frame_name

    ! ------------------------------------------------------------------
    ! Window Divider mode
    ! ------------------------------------------------------------------

    recursive module subroutine window_divider_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%window_divider_mode%command(status)
    end subroutine window_divider_mode

    !> The body of Window Divider mode.  Port of
    !> frame.el:window-divider-mode in part.
    subroutine window_divider_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call window_divider_mode_apply(enable)
    end subroutine window_divider_mode_body

    module subroutine window_divider_mode_apply(enable)
        logical, intent(in) :: enable
        integer :: bottom, right

        bottom = 0
        right = 0

        if (enable) then
            if (window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY .or. &
                window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT) then
                bottom = window_divider_default_bottom_width
            end if

            if (window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY .or. &
                window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT) then
                right = window_divider_default_right_width
            end if
        end if

        ! modify-all-frames-parameters: the frame (a terminal frame only
        ! stores them: frame.c:Fmodify_frame_parameters) and new frames,
        ! where a width of 0 is left out (the default).
        selected_frame%parameters%bottom_divider_width = bottom
        selected_frame%parameters%right_divider_width = right
        default_frame%bottom_divider_width = max(0, bottom)
        default_frame%right_divider_width = max(0, right)
    end subroutine window_divider_mode_apply

    !> Register the frame commands.  Not a port.
    module subroutine initialize_frame_commands()
        commands%make_frame_command%execute => execute_make_frame_command
        commands%other_frame%execute => execute_other_frame
        commands%delete_frame%execute => execute_delete_frame
        commands%delete_other_frames%execute => execute_delete_other_frames
        commands%select_frame_by_name%execute => execute_select_frame_by_name
        commands%set_frame_name%execute => execute_set_frame_name
        call minor_modes%window_divider_mode%define("window_divider_mode", global=.true., &
                                                    hook=window_divider_mode_hook, body=window_divider_mode_body)
        commands%window_divider_mode%execute => window_divider_mode
    end subroutine initialize_frame_commands
end submodule frame_commands_mod
