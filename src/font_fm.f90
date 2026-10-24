! font_fm.f90 -- fonts as redisplay sees them: metrics and the font driver.
! Ported from GNU Emacs 31.1 src/font.h (the parts redisplay uses).
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
! Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> A window-system frame draws text in fonts.  A realized face on such a
!> frame names its font by a font id (`face_type%font', 0 on a terminal),
!> which the frame's font driver (`frame_type%font_driver') resolves to
!> the font's metrics (`font_type') and the metrics of each character
!> (`text_extents').  Redisplay (xdisp_fm) lays out glyphs with these
!> pixel sizes; a terminal frame has no font driver, and every glyph is
!> one column (one "pixel") wide.
!>
!> Only the metrics redisplay needs are ported.  Deferred: font specs and
!> entities, font-related Lisp functions, fontsets, font backends (the
!> GUI step brings one: FreeType rasterizing and HarfBuzz shaping, bound
!> in src/sys), glyph codes other than characters, compositions,
!> underline position and thickness, vertical centering.
module font_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use faces_fm, only: face_attributes_type
    implicit none(type, external)
    private

    !> The metrics of one glyph.  Port of font.h:struct font_metrics.
    type, public :: font_metrics_type
        integer :: lbearing = 0, rbearing = 0, width = 0, ascent = 0, descent = 0
    end type font_metrics_type

    !> The metrics of an opened font, in pixels.  Port of the metrics of
    !> font.h:struct font.
    type, public :: font_type
        !> Minimum and maximum glyph widths.
        integer :: min_width = 0, max_width = 0
        !> The font's size (pixel_size).
        integer :: pixel_size = 0
        !> Height of the font (the line height of text in it).
        integer :: height = 0
        !> Width of the space glyph.
        integer :: space_width = 0
        !> Average width of glyphs (0 if unknown).
        integer :: average_width = 0
        !> Ascent and descent of the font.
        integer :: ascent = 0, descent = 0
        !> Offset of the baseline (baseline_offset).
        integer :: baseline_offset = 0
    end type font_type

    !> A source of fonts: opens the font for a face's attributes and gives
    !> its metrics and those of its characters.  Port of the parts of
    !> font.h:struct font_driver redisplay uses (with font.c's
    !> font_load_for_lface folded into `load_for_lface').
    type, abstract, public :: font_driver_type
    contains
        !> The id (> 0) of the font for the fully specified face
        !> attributes `lface', 0 if there is none (font.c:font_load_for_lface).
        procedure(load_for_lface_proc), deferred :: load_for_lface
        !> The metrics of font `id'.
        procedure(font_proc), deferred :: font
        !> The metrics of character `c' in font `id'; `found' is false if
        !> the font has no glyph for it (encode_char gives FONT_INVALID_CODE).
        procedure(text_extents_proc), deferred :: text_extents
    end type font_driver_type

    abstract interface
        integer function load_for_lface_proc(self, lface) result(id)
            import :: font_driver_type, face_attributes_type
            implicit none(type, external)
            class(font_driver_type), intent(inout) :: self
            type(face_attributes_type), intent(in) :: lface
        end function load_for_lface_proc

        function font_proc(self, id) result(ft)
            import :: font_driver_type, font_type
            implicit none(type, external)
            class(font_driver_type), intent(in) :: self
            integer, intent(in) :: id
            type(font_type) :: ft
        end function font_proc

        subroutine text_extents_proc(self, id, c, metrics, found)
            import :: font_driver_type, font_metrics_type, int32
            implicit none(type, external)
            class(font_driver_type), intent(in) :: self
            integer, intent(in) :: id
            integer(int32), intent(in) :: c
            type(font_metrics_type), intent(out) :: metrics
            logical, intent(out) :: found
        end subroutine text_extents_proc
    end interface
end module font_fm
