! fake_gui_fm.f90 -- a fake window-system output device for the tests.
!
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

!> A window-system frame without a window system, so the tests can check
!> that redisplay does not assume a terminal.  `fake_device_type'
!> implements the redisplay interface by drawing into an in-memory pixel
!> grid: each pixel holds the character and face of the glyph covering
!> it, whether a glyph starts there, and whether a window border is drawn
!> there; the last cursor drawn is kept.  `fake_font_driver_type' has two
!> fonts: a default font 8 pixels wide (wide characters 16) and 16 high,
!> monospaced or proportional (i, j, l, . , ' ! | 4 wide; m, w, M, W, @ 12),
!> and the same font at twice the size, which faces taller than the
!> default (an absolute :height above 1) get.  Not part of the binary.
module fake_gui_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use dispextern_fm, only: redisplay_interface_type, glyph_row_type, STRETCH_GLYPH, NO_CURSOR, DEFAULT_FACE_ID
    use font_fm, only: font_driver_type, font_type, font_metrics_type
    use faces_fm, only: face_attributes_type, FACE_VALUE
    use frame_fm, only: frame_type
    use xdisp_fm, only: glyph_width
    implicit none(type, external)
    private

    !> The font ids of the fake driver.
    integer, parameter, public :: FAKE_DEFAULT_FONT = 1, FAKE_BIG_FONT = 2

    !> The canonical character size of a fake frame (its default font).
    integer, parameter, public :: FAKE_COLUMN_WIDTH = 8, FAKE_LINE_HEIGHT = 16

    !> Fonts for a fake frame.
    type, extends(font_driver_type), public :: fake_font_driver_type
        !> The default font is proportional (else monospaced).
        logical :: proportional = .false.
    contains
        procedure :: load_for_lface => fake_load_for_lface
        procedure :: font => fake_font
        procedure :: text_extents => fake_text_extents
    end type fake_font_driver_type

    !> A pixel grid standing in for a window.
    type, extends(redisplay_interface_type), public :: fake_device_type
        integer :: width = 0, height = 0
        !> Per pixel: the character and face drawn there, whether a glyph
        !> starts there, whether a vertical border is drawn there.
        integer(int32), allocatable :: ch(:, :), face(:, :)
        logical, allocatable :: starts(:, :), border(:, :)
        !> The last cursor drawn.
        integer :: cursor_x = -1, cursor_y = -1, cursor_width = 0, cursor_height = 0, cursor_type = NO_CURSOR
        !> Calls of flush_display and clear_frame.
        integer :: flushes = 0, clears = 0
    contains
        procedure :: clear_frame => fake_clear_frame
        procedure :: write_glyphs => fake_write_glyphs
        procedure :: clear_end_of_line => fake_clear_end_of_line
        procedure :: draw_vertical_window_border => fake_draw_vertical_window_border
        procedure :: draw_window_cursor => fake_draw_window_cursor
        procedure :: flush_display => fake_flush_display
        procedure :: text_at => fake_text_at
        procedure :: column_text => fake_column_text
    end type fake_device_type

    public :: make_fake_frame

contains

    !> Make `f' a fake window-system frame `cols' by `lines' characters
    !> drawn on `device' with fonts from `driver'.
    subroutine make_fake_frame(f, device, driver, cols, lines)
        type(frame_type), intent(inout) :: f
        type(fake_device_type), target, intent(inout) :: device
        type(fake_font_driver_type), target, intent(inout) :: driver
        integer, intent(in) :: cols, lines

        f%rif => device
        f%font_driver => driver
        f%column_width = FAKE_COLUMN_WIDTH
        f%line_height = FAKE_LINE_HEIGHT
        f%smallest_char_width = FAKE_COLUMN_WIDTH
        if (driver%proportional) f%smallest_char_width = 4
        f%smallest_font_height = FAKE_LINE_HEIGHT
        f%text_cols = cols
        f%text_lines = lines
        device%width = cols*FAKE_COLUMN_WIDTH
        device%height = lines*FAKE_LINE_HEIGHT
        allocate (device%ch(0:device%width - 1, 0:device%height - 1), device%face(0:device%width - 1, 0:device%height - 1), &
                  device%starts(0:device%width - 1, 0:device%height - 1), &
                  device%border(0:device%width - 1, 0:device%height - 1))
        device%ch = 32
        device%face = DEFAULT_FACE_ID
        device%starts = .false.
        device%border = .false.
    end subroutine make_fake_frame

    integer function fake_load_for_lface(self, lface) result(id)
        class(fake_font_driver_type), intent(inout) :: self
        type(face_attributes_type), intent(in) :: lface

        id = FAKE_DEFAULT_FONT

        if (lface%height%state == FACE_VALUE) then
            if (lface%height%absolute > 1) id = FAKE_BIG_FONT
        end if
    end function fake_load_for_lface

    function fake_font(self, id) result(ft)
        class(fake_font_driver_type), intent(in) :: self
        integer, intent(in) :: id
        type(font_type) :: ft
        integer :: scale

        scale = 1
        if (id == FAKE_BIG_FONT) scale = 2
        ft%pixel_size = 16*scale
        ft%height = 16*scale
        ft%ascent = 12*scale
        ft%descent = 4*scale
        ft%space_width = 8*scale
        ft%average_width = 8*scale
        ft%min_width = 8*scale
        if (self%proportional) ft%min_width = 4*scale
        ft%max_width = 16*scale
    end function fake_font

    subroutine fake_text_extents(self, id, c, metrics, found)
        class(fake_font_driver_type), intent(in) :: self
        integer, intent(in) :: id
        integer(int32), intent(in) :: c
        type(font_metrics_type), intent(out) :: metrics
        logical, intent(out) :: found
        integer :: scale

        scale = 1
        if (id == FAKE_BIG_FONT) scale = 2
        found = .true.
        metrics%width = 8

        if (glyph_width(c) == 2) then
            metrics%width = 16
        else if (self%proportional) then
            if (c < 128) then
                if (index("ijl.,'!|", achar(c)) > 0) metrics%width = 4
                if (index("mwMW@", achar(c)) > 0) metrics%width = 12
            end if
        end if

        metrics%width = metrics%width*scale
        metrics%lbearing = 0
        metrics%rbearing = metrics%width
        metrics%ascent = 12*scale
        metrics%descent = 4*scale
    end subroutine fake_text_extents

    !> Blank the rectangle [x0, x1) x [y0, y1) of the grid.
    subroutine blank(self, x0, x1, y0, y1)
        class(fake_device_type), intent(inout) :: self
        integer, intent(in) :: x0, x1, y0, y1
        integer :: a, b, c, d

        a = max(0, x0)
        b = min(self%width, x1) - 1
        c = max(0, y0)
        d = min(self%height, y1) - 1
        if (a > b .or. c > d) return
        self%ch(a:b, c:d) = 32
        self%face(a:b, c:d) = DEFAULT_FACE_ID
        self%starts(a:b, c:d) = .false.
        self%border(a:b, c:d) = .false.
    end subroutine blank

    subroutine fake_clear_frame(self)
        class(fake_device_type), intent(inout) :: self

        call blank(self, 0, self%width, 0, self%height)
        self%cursor_type = NO_CURSOR
        self%clears = self%clears + 1
    end subroutine fake_clear_frame

    subroutine fake_write_glyphs(self, row, start, len, x, y, clip_x)
        class(fake_device_type), intent(inout) :: self
        type(glyph_row_type), intent(in) :: row
        integer, intent(in) :: start, len, x, y, clip_x
        integer :: k, gx, x1, c, d
        integer(int32) :: ch

        gx = x
        c = max(0, y)
        d = min(self%height, y + row%height) - 1

        do k = start, start + len - 1
            x1 = min(gx + row%glyphs(k)%pixel_width, clip_x, self%width)

            if (gx >= 0 .and. x1 > gx .and. c <= d) then
                ch = row%glyphs(k)%ch
                if (row%glyphs(k)%type == STRETCH_GLYPH) ch = 32
                self%ch(gx:x1 - 1, c:d) = ch
                self%face(gx:x1 - 1, c:d) = row%glyphs(k)%face_id
                self%starts(gx:x1 - 1, c:d) = .false.
                self%starts(gx, c:d) = .true.
                self%border(gx:x1 - 1, c:d) = .false.
            end if

            gx = gx + row%glyphs(k)%pixel_width
        end do
    end subroutine fake_write_glyphs

    subroutine fake_clear_end_of_line(self, x, y, height, to_x)
        class(fake_device_type), intent(inout) :: self
        integer, intent(in) :: x, y, height, to_x

        call blank(self, x, to_x, y, y + height)
    end subroutine fake_clear_end_of_line

    subroutine fake_draw_vertical_window_border(self, x, y0, y1, face_id)
        class(fake_device_type), intent(inout) :: self
        integer, intent(in) :: x, y0, y1
        integer(int32), intent(in) :: face_id

        if (x < 0 .or. x >= self%width) return
        self%border(x, max(0, y0):min(self%height - 1, y1)) = .true.
        self%face(x, max(0, y0):min(self%height - 1, y1)) = face_id
    end subroutine fake_draw_vertical_window_border

    subroutine fake_draw_window_cursor(self, x, y, width, height, cursor_type, on_p, active_p)
        class(fake_device_type), intent(inout) :: self
        integer, intent(in) :: x, y, width, height, cursor_type
        logical, intent(in) :: on_p, active_p

        if (.not. on_p .or. .not. active_p) return
        self%cursor_x = x
        self%cursor_y = y
        self%cursor_width = width
        self%cursor_height = height
        self%cursor_type = cursor_type
    end subroutine fake_draw_window_cursor

    subroutine fake_flush_display(self)
        class(fake_device_type), intent(inout) :: self

        self%flushes = self%flushes + 1
    end subroutine fake_flush_display

    !> The characters of the glyphs starting on pixel row `y' between `x0'
    !> and `x1' (the whole width without them), left to right; ? for
    !> non-ASCII characters.
    function fake_text_at(self, y, x0, x1) result(s)
        class(fake_device_type), intent(in) :: self
        integer, intent(in) :: y
        integer, intent(in), optional :: x0, x1
        character(len=:), allocatable :: s
        integer :: x, a, b

        a = 0
        b = self%width - 1
        if (present(x0)) a = x0
        if (present(x1)) b = x1 - 1
        s = ""

        do x = a, b
            if (.not. self%starts(x, y)) cycle

            if (self%ch(x, y) < 128) then
                s = s//achar(self%ch(x, y))
            else
                s = s//"?"
            end if
        end do
    end function fake_text_at

    !> The character covering the first pixel of each of `n' columns of
    !> pixel row `y' (a character cell grid read off the pixels), as code
    !> points.
    function fake_column_text(self, y, n) result(codes)
        class(fake_device_type), intent(in) :: self
        integer, intent(in) :: y, n
        integer(int32) :: codes(n)
        integer :: k

        do k = 1, n
            codes(k) = self%ch((k - 1)*FAKE_COLUMN_WIDTH, y)
        end do
    end function fake_column_text
end module fake_gui_fm
