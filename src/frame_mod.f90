! frame_mod.f90 -- frames: a terminal screen with its glyph matrices.
! Ported from GNU Emacs 31.1 src/frame.h (struct frame, the parts in use).
!
! Copyright (C) 1993-1994, 1999-2026 Free Software Foundation, Inc.
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

!> One frame per terminal, so a frame holds its terminal directly.  The
!> current matrix is what the screen shows; redisplay fills the desired
!> matrix, and update_frame (dispnew_mod) makes the screen match it.
!> The frame has a window tree (its root window), the selected window (a
!> live window of the tree) and a minibuffer window on its last line, all
!> windows of window_mod's registry; child frames are not ported.
!>
!> Sizes are in pixels where upstream has them: FRAME_COLUMN_WIDTH and
!> FRAME_LINE_HEIGHT are 1 on a terminal frame, the default font's width
!> and height on a window-system frame (one with a redisplay interface,
!> `rif', and a font driver).  Windows are sized in columns and lines
!> (`frame_resize_pixelwise' and `window_resize_pixelwise' .false.), so their
!> pixel edges are those times the frame's column width and line height.
!> Window-system frames exist only in the tests (a fake device) until the
!> GUI is ported.
module frame_mod
    use termchar_mod, only: tty_display_info_type
    use dispextern_mod, only: glyph_matrix_type, face_type, redisplay_interface_type
    use font_mod, only: font_driver_type
    use handle_mod, only: handle_type
    use window_mod, only: window_type, window_ref, is_window_valid, make_window, windows, window_text_cols, &
                          window_safe_min_height, window_safe_min_width, window_resize_root_window, &
                          window_resize_check, window_resize_apply
    implicit none(type, external)
    private

    !> Port of the parts of frame.h:struct frame in use.
    type, public :: frame_type
        type(tty_display_info_type) :: tty
        type(glyph_matrix_type) :: current_matrix, desired_matrix
        !> The root of the window tree, the minibuffer window and the
        !> selected window (frame.h root_window, minibuffer_window,
        !> selected_window).
        type(handle_type) :: root_window, minibuffer_window, selected_window
        !> The realized faces, indexed by face id (FRAME_FACE_CACHE).
        type(face_type), allocatable :: faces(:)
        !> Where to leave the cursor after an update (set by redisplay from
        !> the selected window's cursor), 0-based; -1 for none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> The minibuffer window is the selected window (while a minibuffer
        !> is active; `selected_window' is the window it was entered from).
        logical :: minibuffer_selected = .false.
        !> The screen contents are unknown and must be redrawn
        !> (frame.h:garbaged, set by frame.h:SET_FRAME_GARBAGED).
        logical :: needs_redraw = .true.
        logical :: glyphs_initialized_p = .false.
        !> How redisplay draws on a window-system frame (FRAME_RIF); not
        !> associated on a terminal frame.
        class(redisplay_interface_type), pointer :: rif => null()
        !> The fonts of a window-system frame's faces (f->font_driver_list).
        class(font_driver_type), pointer :: font_driver => null()
        !> FRAME_COLUMN_WIDTH and FRAME_LINE_HEIGHT: the canonical
        !> character width and line height in pixels.
        integer :: column_width = 1, line_height = 1
        !> The narrowest glyph and the lowest font of the frame's display
        !> (FRAME_SMALLEST_CHAR_WIDTH, FRAME_SMALLEST_FONT_HEIGHT), which
        !> bound how many glyphs and rows a window can show.
        integer :: smallest_char_width = 1, smallest_font_height = 1
        !> The size of a window-system frame in columns and lines
        !> (text_cols, text_lines); a terminal frame has its terminal's.
        integer :: text_cols = 0, text_lines = 0
        !> A size change requested but not made yet (delayed by
        !> dispnew_mod's `change_frame_size'): the new width and height in
        !> pixels, -1 for no change, and whether one is pending
        !> (frame.h:new_width, frame.h:new_height, frame.h:new_size_p).
        integer :: new_width = -1, new_height = -1
        logical :: new_size_pending = .false.
    end type frame_type

    !> The terminal frame (frame.c:selected_frame).
    type(frame_type), target, public :: selected_frame

    public :: ensure_frame_windows, selected_window, selected_window_ref, frame_selected_window_ref, minibuffer_window_ref
    public :: frame_lines, frame_cols, window_pixel_left, window_pixel_top, window_pixel_width, &
              window_pixel_height, window_body_pixel_width
    public :: frame_windows_min_size, resize_frame_windows

contains

    !> The frame's height in lines (frame.h:FRAME_LINES): its terminal's
    !> on a terminal frame.  Port of frame.h:FRAME_LINES.
    integer function frame_lines(f)
        type(frame_type), intent(in) :: f

        if (associated(f%rif)) then
            frame_lines = f%text_lines
        else
            frame_lines = f%tty%wcm%cm_rows
        end if
    end function frame_lines

    !> The frame's width in columns.  Port of frame.h:FRAME_COLS.
    integer function frame_cols(f)
        type(frame_type), intent(in) :: f

        if (associated(f%rif)) then
            frame_cols = f%text_cols
        else
            frame_cols = f%tty%wcm%cm_cols
        end if
    end function frame_cols

    !> The left edge of window `w' on frame `f' in pixels.
    !> Port of window.h:WINDOW_LEFT_EDGE_X, in part.
    integer function window_pixel_left(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_left = w%left_col*f%column_width
    end function window_pixel_left

    !> The top edge of window `w' in pixels.  Port of
    !> window.h:WINDOW_TOP_EDGE_Y, in part.
    integer function window_pixel_top(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_top = w%top_line*f%line_height
    end function window_pixel_top

    !> The width of window `w' in pixels.  Port of
    !> window.h:WINDOW_PIXEL_WIDTH, in part.
    integer function window_pixel_width(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_width = w%total_cols*f%column_width
    end function window_pixel_width

    !> The height of window `w' in pixels, mode line included.
    !> Port of window.h:WINDOW_PIXEL_HEIGHT, in part.
    integer function window_pixel_height(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_height = w%total_lines*f%line_height
    end function window_pixel_height

    !> The width of the text area of window `w' in pixels: on a terminal,
    !> less the column of the border with a window on its right; a
    !> window-system frame draws that border over the window's last pixel
    !> column (there are no fringes, margins, scroll bars or dividers).
    !> Port of window.c:window_body_width (WINDOW_BODY_IN_PIXELS).
    integer function window_body_pixel_width(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        if (associated(f%rif)) then
            window_body_pixel_width = window_pixel_width(f, w)
        else
            window_body_pixel_width = window_text_cols(w)
        end if
    end function window_body_pixel_width

    !> Return minimum number of lines of FRAME's windows.
    !> HORIZONTAL true means return number of columns of FRAME's windows.
    !> Sizes are what the windows can safely shrink to (IGNORE `safe'),
    !> in lines and columns.  Port of window.el:frame-windows-min-size
    !> (IGNORE `safe', as frame.c:adjust_frame_size asks for it when a
    !> frame size change comes from the terminal), with
    !> window.el:window-min-size, window.el:window--min-size-1 and
    !> window.el:window-safe-min-size (`window_size_fixed' is not ported).
    integer function frame_windows_min_size(f, horizontal) result(n)
        type(frame_type), intent(in) :: f
        logical, intent(in) :: horizontal

        n = safe_min_size(f%root_window)
        ! The minibuffer window: one line (window.el:window-min-pixel-height).
        if (.not. horizontal .and. is_window_valid(f%minibuffer_window)) n = n + max(1, window_safe_min_height)

    contains

        !> The safe minimum size of the tree under `h'.
        recursive integer function safe_min_size(h) result(m)
            type(handle_type), intent(in) :: h
            type(handle_type) :: sub

            m = 0
            if (.not. is_window_valid(h)) return
            sub = windows(h%index)%child

            if (.not. is_window_valid(sub)) then
                m = window_safe_min_height
                if (horizontal) m = window_safe_min_width
            else if (windows(h%index)%horizontal .eqv. horizontal) then
                ! An iso-combination: the sum of its children's sizes.
                do while (is_window_valid(sub))
                    m = m + safe_min_size(sub)
                    sub = windows(sub%index)%next
                end do
            else
                ! An ortho-combination: the largest of its children's sizes.
                do while (is_window_valid(sub))
                    m = max(m, safe_min_size(sub))
                    sub = windows(sub%index)%next
                end do
            end if
        end function safe_min_size
    end function frame_windows_min_size

    !> Resize frame F's windows when F's inner height (inner width if
    !> HORFLAG is true) has been set to SIZE pixels: the root window takes
    !> the frame's new size less the minibuffer window, which becomes one
    !> line high at the bottom.  A root window with children shares the
    !> change among them by their normal and minimum sizes
    !> (`window_resize_root_window'), first respecting `window_min_height'
    !> and `window_min_width', then ignoring them; when neither works the
    !> windows keep their sizes.  Port of window.c:resize_frame_windows
    !> (no tab line or menu bar: the root window starts at the top; window
    !> sizes are whole lines and columns, so window.el's
    !> window--pixel-to-total has nothing to round).
    subroutine resize_frame_windows(f, size, horflag)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: size
        logical, intent(in) :: horflag
        integer :: unit, old_size, old_pixel_size, new_units, new_pixel_size, mini_height, frame_cols
        type(handle_type) :: root, mini

        root = f%root_window
        mini = f%minibuffer_window
        if (.not. is_window_valid(root)) return

        if (horflag) then
            unit = f%column_width
            old_size = windows(root%index)%total_cols
        else
            unit = f%line_height
            old_size = windows(root%index)%total_lines
        end if

        old_pixel_size = old_size*unit
        ! The minibuffer window has no mode line, so all of it is its
        ! body: one line remains.
        mini_height = 0
        if (is_window_valid(mini)) mini_height = unit

        if (horflag) then
            new_pixel_size = max(size, unit)
        else
            new_pixel_size = max(size - mini_height, unit)
        end if

        new_units = new_pixel_size/unit
        frame_cols = windows(root%index)%total_cols
        if (horflag) frame_cols = new_units

        if (new_pixel_size == old_pixel_size .and. (horflag .or. windows(root%index)%top_line == 0)) then
            ! Nothing to do for the root window.
        else if (.not. is_window_valid(windows(root%index)%child)) then
            ! For a leaf root window just set the size.
            if (horflag) then
                windows(root%index)%total_cols = new_units
                windows(root%index)%divider = .false.
            else
                windows(root%index)%top_line = 0
                windows(root%index)%total_lines = new_units
            end if
        else
            if (.not. horflag) windows(root%index)%top_line = 0
            ! Try a "normal" resize first.
            call window_resize_root_window(root, new_units - old_size, horflag, .false.)

            if (window_resize_check(root, horflag) .and. new_units == windows(root%index)%new_total) then
                call window_resize_apply(root, horflag, frame_cols)
            else
                ! Try with "reasonable" minimum sizes next.
                call window_resize_root_window(root, new_units - old_size, horflag, .true.)

                if (window_resize_check(root, horflag) .and. new_units == windows(root%index)%new_total) then
                    call window_resize_apply(root, horflag, frame_cols)
                end if
            end if
        end if

        if (.not. is_window_valid(mini)) return

        associate (m => windows(mini%index), r => windows(root%index))
            if (horflag) then
                m%total_cols = new_units
            else
                m%total_lines = mini_height/unit
                m%top_line = r%top_line + r%total_lines
            end if
        end associate
    end subroutine resize_frame_windows

    !> Make `selected_frame''s windows if they don't exist yet: one window
    !> (the root, selected) and the minibuffer window, sized later by
    !> xdisp_mod:init_frame_windows.  Port of window.c:init_window_once
    !> (the initial frame's windows).
    subroutine ensure_frame_windows()
        if (.not. is_window_valid(selected_frame%root_window)) then
            call make_window(selected_frame%root_window)
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. is_window_valid(selected_frame%selected_window)) then
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. is_window_valid(selected_frame%minibuffer_window)) then
            call make_window(selected_frame%minibuffer_window)
            windows(selected_frame%minibuffer_window%index)%mini = .true.
        end if
    end subroutine ensure_frame_windows

    !> Return the selected window.  The selected window is the window in
    !> which the standard cursor for selected windows appears and to
    !> which many commands apply.  Port of window.c:Fselected_window.
    function selected_window() result(h)
        type(handle_type) :: h

        call ensure_frame_windows()

        if (selected_frame%minibuffer_selected) then
            h = selected_frame%minibuffer_window
        else
            h = selected_frame%selected_window
        end if
    end function selected_window

    !> The selected window of `selected_frame': the minibuffer window while
    !> it is selected, else `selected_window'.  window.c:selected_window.
    !> Not a port.
    function selected_window_ref() result(w)
        type(window_type), pointer :: w

        if (selected_frame%minibuffer_selected) then
            w => minibuffer_window_ref()
        else
            w => frame_selected_window_ref()
        end if
    end function selected_window_ref

    !> The frame's selected window (not the minibuffer window): the one
    !> showing the current buffer, or that the minibuffer was entered from.
    !> Valid until a window is made.  Not a port.
    function frame_selected_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%selected_window)
    end function frame_selected_window_ref

    !> The minibuffer window.  Valid until a window is made.  Not a port.
    function minibuffer_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%minibuffer_window)
    end function minibuffer_window_ref
end module frame_mod
