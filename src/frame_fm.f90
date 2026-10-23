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
module frame_fm
    use termchar_fm, only: tty_display_info
    use dispextern_fm, only: glyph_matrix_type, face_type
    use handle_fm, only: handle_type
    use window_fm, only: window_type, window_ref, window_valid_p, make_window, windows
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
    end type frame_type

    !> The terminal frame (frame.c:selected_frame).
    type(frame_type), target, public :: selected_frame

    public :: ensure_frame_windows, selected_window, selected_window_ref, frame_selected_window_ref, minibuffer_window_ref

contains

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
