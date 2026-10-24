! dispextern_fm.f90 -- display structures shared by the display code.
! Ported from GNU Emacs 31.1 src/dispextern.h (the parts in use).
!
! Copyright (C) 1985, 1993-1994, 1997-2026 Free Software Foundation, Inc.
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

!> A glyph is a character (or a stretch of space) and the face to draw it
!> in, with its size in pixels.  On a terminal every glyph is one
!> character cell, one "pixel" wide (a wide character takes a glyph and a
!> padding glyph), and every row one line high.  On a window-system frame
!> glyphs and rows take the sizes of their faces' fonts, and output goes
!> through the frame's redisplay interface (`redisplay_interface_type').
!> Faces are numbered; display code passes the frame's face table
!> (indexed from DEFAULT_FACE_ID = 0) to the terminal functions.
module dispextern_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use faces_fm, only: face_attributes_type
    implicit none(type, external)
    private

    !> dispextern.h: FACE_TTY_DEFAULT_COLOR and friends (unsigned -1, -2,
    !> -3 upstream).  Real colors are >= 0.
    integer(int64), parameter, public :: FACE_TTY_DEFAULT_COLOR = -1, &
                                         FACE_TTY_DEFAULT_FG_COLOR = -2, &
                                         FACE_TTY_DEFAULT_BG_COLOR = -3

    !> enum face_underline_type (the order of terminfo's Smulx).
    integer, parameter, public :: FACE_NO_UNDERLINE = 0, &
                                  FACE_UNDERLINE_SINGLE = 1, &
                                  FACE_UNDERLINE_DOUBLE_LINE = 2, &
                                  FACE_UNDERLINE_WAVE = 3, &
                                  FACE_UNDERLINE_DOTS = 4, &
                                  FACE_UNDERLINE_DASHES = 5

    !> enum face_id: the basic faces, at fixed ids in every frame's face
    !> cache; faces realized later get ids from BASIC_FACE_ID_SENTINEL.
    integer(int32), parameter, public :: DEFAULT_FACE_ID = 0, &
                                         MODE_LINE_ACTIVE_FACE_ID = 1, &
                                         MODE_LINE_INACTIVE_FACE_ID = 2, &
                                         TOOL_BAR_FACE_ID = 3, &
                                         FRINGE_FACE_ID = 4, &
                                         HEADER_LINE_ACTIVE_FACE_ID = 5, &
                                         HEADER_LINE_INACTIVE_FACE_ID = 6, &
                                         SCROLL_BAR_FACE_ID = 7, &
                                         BORDER_FACE_ID = 8, &
                                         CURSOR_FACE_ID = 9, &
                                         MOUSE_FACE_ID = 10, &
                                         MENU_FACE_ID = 11, &
                                         VERTICAL_BORDER_FACE_ID = 12, &
                                         WINDOW_DIVIDER_FACE_ID = 13, &
                                         WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID = 14, &
                                         WINDOW_DIVIDER_LAST_PIXEL_FACE_ID = 15, &
                                         INTERNAL_BORDER_FACE_ID = 16, &
                                         CHILD_FRAME_BORDER_FACE_ID = 17, &
                                         TAB_BAR_FACE_ID = 18, &
                                         TAB_LINE_ACTIVE_FACE_ID = 19, &
                                         TAB_LINE_INACTIVE_FACE_ID = 20, &
                                         MARGIN_FACE_ID = 21, &
                                         BASIC_FACE_ID_SENTINEL = 22

    !> Terminal capabilities a face may need (dispextern.h TTY_CAP_*),
    !> for term.c:tty_capable_p.
    integer, parameter, public :: TTY_CAP_INVERSE = 2, TTY_CAP_UNDERLINE = 4, TTY_CAP_BOLD = 8, &
                                  TTY_CAP_DIM = 16, TTY_CAP_ITALIC = 32, TTY_CAP_STRIKE_THROUGH = 64, &
                                  TTY_CAP_UNDERLINE_STYLED = 128

    !> enum glyph_type: the kinds of glyphs in use.
    integer, parameter, public :: CHAR_GLYPH = 0, STRETCH_GLYPH = 4

    !> enum text_cursor_kinds.
    integer, parameter, public :: DEFAULT_CURSOR = -2, NO_CURSOR = -1, FILLED_BOX_CURSOR = 0, &
                                  HOLLOW_BOX_CURSOR = 1, BAR_CURSOR = 2, HBAR_CURSOR = 3

    !> The parts of struct glyph in use.
    type, public :: glyph_type
        integer(int32) :: ch = 32
        integer(int32) :: face_id = DEFAULT_FACE_ID
        !> CHAR_GLYPH_PADDING_P: continuation cell of a wide character.
        logical :: padding = .false.
        !> CHAR_GLYPH or STRETCH_GLYPH (struct glyph type).
        integer :: type = CHAR_GLYPH
        !> Width in pixels (1 on a terminal), and the ascent and descent
        !> of the glyph above and below the baseline (0 on a terminal).
        integer :: pixel_width = 1, ascent = 0, descent = 0
    end type glyph_type

    !> The terminal parts of struct face.
    type, public :: face_type
        integer(int64) :: foreground = FACE_TTY_DEFAULT_COLOR
        integer(int64) :: background = FACE_TTY_DEFAULT_COLOR
        logical :: tty_bold_p = .false., tty_italic_p = .false., &
                   tty_reverse_p = .false., tty_strike_through_p = .false.
        integer :: underline = FACE_NO_UNDERLINE
        !> 0 for the default underline color.
        integer(int64) :: underline_color = 0
        !> The fully specified attributes the face was realized from
        !> (struct face's lface).
        type(face_attributes_type) :: lface
        !> The face's font on a window-system frame, an id of the frame's
        !> font driver (struct face's font); 0 on a terminal.
        integer :: font = 0
    end type face_type

    !> The text area of one screen line (struct glyph_row).  `glyphs'
    !> holds a full line; the first `used' are meaningful.
    type, public :: glyph_row_type
        type(glyph_type), allocatable :: glyphs(:)
        integer :: used = 0
        !> The row holds valid glyphs (false: unknown / not to be drawn).
        logical :: enabled_p = .false.
        !> In pixels: where the row is in its window (x of the first glyph,
        !> y of its top), its width, its ascent (the baseline's distance
        !> from the top), its height and the part of it visible in the
        !> window.  On a terminal x is 0, y the line, the ascent 0 and the
        !> heights 1.
        integer :: x = 0, y = 0, pixel_width = 0, ascent = 0, height = 1, visible_height = 1
        !> The line continues on the next row (continued_p); the row ends
        !> at the end of the accessible text (ends_at_zv_p).
        logical :: continued_p = .false., ends_at_zv_p = .false.
    end type glyph_row_type

    !> A grid of rows (struct glyph_matrix); rows are numbered from 0.
    type, public :: glyph_matrix_type
        type(glyph_row_type), allocatable :: rows(:)
        integer :: nrows = 0, ncols = 0
    end type glyph_matrix_type

    !> How redisplay draws on a window-system frame: the output hooks of
    !> its terminal (the parts of dispextern.h:struct redisplay_interface,
    !> and termhooks.h's clear_frame_hook, in use).  Coordinates are
    !> pixels from the top left corner of the frame; upstream passes the
    !> window and converts (WINDOW_TO_FRAME_PIXEL_X).  A terminal frame has
    !> none (FRAME_RIF is NULL) and is written through term_fm.
    type, abstract, public :: redisplay_interface_type
    contains
        !> Clear the whole frame (clear_frame_hook).
        procedure(rif_clear_frame), deferred :: clear_frame
        !> Draw `len' glyphs of `row' from glyph `start', the first with its
        !> left edge at `x', the row's top at `y', clipped at `clip_x'
        !> (the right edge of the window) (write_glyphs).
        procedure(rif_write_glyphs), deferred :: write_glyphs
        !> Clear from `x' to `to_x' (exclusive) the `height' pixels of a
        !> row starting at `y' (clear_end_of_line).
        procedure(rif_clear_end_of_line), deferred :: clear_end_of_line
        !> Draw the border between side-by-side windows from (x, y0) to
        !> (x, y1) in face `face_id' (draw_vertical_window_border).
        procedure(rif_draw_vertical_window_border), deferred :: draw_vertical_window_border
        !> Draw a cursor of kind `cursor_type' (FILLED_BOX_CURSOR, ...)
        !> `width' by `height' at (x, y) if `on_p' (draw_window_cursor).
        procedure(rif_draw_window_cursor), deferred :: draw_window_cursor
        !> Make what was drawn visible (flush_display).
        procedure(rif_flush_display), deferred :: flush_display
    end type redisplay_interface_type

    abstract interface
        subroutine rif_clear_frame(self)
            import :: redisplay_interface_type
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
        end subroutine rif_clear_frame

        subroutine rif_write_glyphs(self, row, start, len, x, y, clip_x)
            import :: redisplay_interface_type, glyph_row_type
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
            type(glyph_row_type), intent(in) :: row
            integer, intent(in) :: start, len, x, y, clip_x
        end subroutine rif_write_glyphs

        subroutine rif_clear_end_of_line(self, x, y, height, to_x)
            import :: redisplay_interface_type
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
            integer, intent(in) :: x, y, height, to_x
        end subroutine rif_clear_end_of_line

        subroutine rif_draw_vertical_window_border(self, x, y0, y1, face_id)
            import :: redisplay_interface_type, int32
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
            integer, intent(in) :: x, y0, y1
            integer(int32), intent(in) :: face_id
        end subroutine rif_draw_vertical_window_border

        subroutine rif_draw_window_cursor(self, x, y, width, height, cursor_type, on_p, active_p)
            import :: redisplay_interface_type
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
            integer, intent(in) :: x, y, width, height, cursor_type
            logical, intent(in) :: on_p, active_p
        end subroutine rif_draw_window_cursor

        subroutine rif_flush_display(self)
            import :: redisplay_interface_type
            implicit none(type, external)
            class(redisplay_interface_type), intent(inout) :: self
        end subroutine rif_flush_display
    end interface

    public :: face_tty_specified_color, glyph_equal_p, char_glyph_space_p

contains

    !> Whether `color' is a real color rather than a default.
    !> Port of dispextern.h:face_tty_specified_color.
    elemental logical function face_tty_specified_color(color)
        integer(int64), intent(in) :: color
        face_tty_specified_color = color >= 0
    end function face_tty_specified_color

    !> dispextern.h:GLYPH_EQUAL_P for character and stretch glyphs.
    elemental logical function glyph_equal_p(x, y)
        type(glyph_type), intent(in) :: x, y
        glyph_equal_p = x%type == y%type .and. x%ch == y%ch .and. x%face_id == y%face_id .and. &
                        (x%padding .eqv. y%padding) .and. x%pixel_width == y%pixel_width
    end function glyph_equal_p

    !> A blank in the default face.  dispextern.h:CHAR_GLYPH_SPACE_P.
    elemental logical function char_glyph_space_p(g)
        type(glyph_type), intent(in) :: g
        char_glyph_space_p = g%ch == 32 .and. g%face_id == DEFAULT_FACE_ID
    end function char_glyph_space_p
end module dispextern_fm
