! frame_fm.f90 -- frames: a terminal screen with its glyph matrices.
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
!> matrix, and update_frame (dispnew_fm) makes the screen match it.
!> The frame has a window tree (its root window), the selected window (a
!> live window of the tree) and a minibuffer window on its last line, all
!> windows of window_fm's registry; child frames are not ported.
!>
!> Sizes are in pixels where upstream has them: FRAME_COLUMN_WIDTH and
!> FRAME_LINE_HEIGHT are 1 on a terminal frame, the default font's width
!> and height on a window-system frame (one with a redisplay interface,
!> `rif', and a font driver).  Windows are sized in columns and lines
!> (frame-resize-pixelwise and window-resize-pixelwise nil), so their
!> pixel edges are those times the frame's column width and line height.
!> Window-system frames exist only in the tests (a fake device) until the
!> GUI is ported.
module frame_fm
    use termchar_fm, only: tty_display_info
    use dispextern_fm, only: glyph_matrix_type, face_type, redisplay_interface_type
    use font_fm, only: font_driver_type
    use handle_fm, only: handle_type
    use window_fm, only: window_type, window_ref, window_valid_p, make_window, windows, window_text_cols
    implicit none(type, external)
    private

    !> Port of the parts of frame.h:struct frame in use.
    type, public :: frame_type
        type(tty_display_info) :: tty
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
        !> Hide the cursor after the update (cursor_off_p).
        logical :: cursor_off_p = .false.
        !> The minibuffer window is the selected window (while a minibuffer
        !> is active; `selected_window' is the window it was entered from).
        logical :: minibuffer_selected = .false.
        !> The screen contents are unknown and must be redrawn.
        logical :: garbaged = .true.
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
    end type frame_type

    !> The terminal frame (frame.c:selected_frame).
    type(frame_type), target, public :: selected_frame

    public :: ensure_frame_windows, selected_window, selected_window_ref, frame_selected_window_ref, minibuffer_window_ref
    public :: frame_window_p, frame_lines, frame_cols, window_pixel_left, window_pixel_top, window_pixel_width, &
              window_pixel_height, window_body_pixel_width

contains

    !> Whether `f' is a window-system frame.  frame.h:FRAME_WINDOW_P.
    logical function frame_window_p(f)
        type(frame_type), intent(in) :: f

        frame_window_p = associated(f%rif)
    end function frame_window_p

    !> The frame's height in lines (frame.h:FRAME_LINES): its terminal's
    !> on a terminal frame.
    integer function frame_lines(f)
        type(frame_type), intent(in) :: f

        if (frame_window_p(f)) then
            frame_lines = f%text_lines
        else
            frame_lines = f%tty%wcm%cm_rows
        end if
    end function frame_lines

    !> The frame's width in columns (frame.h:FRAME_COLS).
    integer function frame_cols(f)
        type(frame_type), intent(in) :: f

        if (frame_window_p(f)) then
            frame_cols = f%text_cols
        else
            frame_cols = f%tty%wcm%cm_cols
        end if
    end function frame_cols

    !> The left edge of window `w' on frame `f' in pixels
    !> (window.h:WINDOW_LEFT_EDGE_X).
    integer function window_pixel_left(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_left = w%left_col*f%column_width
    end function window_pixel_left

    !> The top edge of window `w' in pixels (window.h:WINDOW_TOP_EDGE_Y).
    integer function window_pixel_top(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_top = w%top_line*f%line_height
    end function window_pixel_top

    !> The width of window `w' in pixels (window.h:WINDOW_PIXEL_WIDTH).
    integer function window_pixel_width(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_width = w%total_cols*f%column_width
    end function window_pixel_width

    !> The height of window `w' in pixels, mode line included
    !> (window.h:WINDOW_PIXEL_HEIGHT).
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

        if (frame_window_p(f)) then
            window_body_pixel_width = window_pixel_width(f, w)
        else
            window_body_pixel_width = window_text_cols(w)
        end if
    end function window_body_pixel_width

    !> Make `selected_frame''s windows if they don't exist yet: one window
    !> (the root, selected) and the minibuffer window, sized later by
    !> xdisp_fm:init_frame_windows.
    subroutine ensure_frame_windows()
        if (.not. window_valid_p(selected_frame%root_window)) then
            call make_window(selected_frame%root_window)
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. window_valid_p(selected_frame%selected_window)) then
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. window_valid_p(selected_frame%minibuffer_window)) then
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
    !> Valid until a window is made.
    function frame_selected_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%selected_window)
    end function frame_selected_window_ref

    !> The minibuffer window.  Valid until a window is made.
    function minibuffer_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%minibuffer_window)
    end function minibuffer_window_ref
end module frame_fm
