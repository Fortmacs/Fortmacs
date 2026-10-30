! color_fm.f90 -- color manipulation library.
! Ported from GNU Emacs 31.1 lisp/color.el.
!
! Copyright (C) 2010-2026 Free Software Foundation, Inc.
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

!> Color conversions: `color_name_to_rgb', `color_rgb_to_hex' and
!> `color_hsl_to_rgb'.
!>
!> Deferred: the rest of color.el (the HSV, CIE XYZ and CIE L*a*b*
!> conversions, `color_complement', `color_distance' helpers,
!> `color_saturate_name', `color_lighten_name', `color_blend',
!> `color_gradient' and the others).
module color_fm
    use, intrinsic :: iso_fortran_env, only: real64
    use faces_fm, only: color_values
    implicit none(type, external)
    private

    public :: color_name_to_rgb, color_rgb_to_hex, color_hue_to_rgb, color_hsl_to_rgb

contains

    !> Convert COLOR string to an array of normalized RGB components.
    !> COLOR should be a color name (e.g. "white") or an RGB triplet
    !> string (e.g. "#ffff1122eecc").
    !>
    !> COLOR can also be one of the strings "unspecified-fg" or
    !> "unspecified-bg", in which case `found' is false.
    !>
    !> Normally the result is three floating-point numbers, (RED GREEN
    !> BLUE), each between 0.0 and 1.0 inclusive.
    !>
    !> If the selected frame cannot display COLOR, `found' is false.  Port
    !> of color.el:color-name-to-rgb.
    subroutine color_name_to_rgb(color, rgb, found)
        character(len=*), intent(in) :: color
        real(real64), intent(out) :: rgb(3)
        logical, intent(out) :: found
        integer :: r, g, b, valmax
        logical :: ok

        rgb = 0
        ! `color_values' maximum value is either 65535 or 65280 depending on
        ! the display system.  So we use a white conversion to get the max
        ! value.
        call color_values("#ffffffffffff", ok, valmax, g, b)
        call color_values(color, found, r, g, b)
        if (.not. found .or. .not. ok) return
        rgb = [r, g, b]/real(valmax, real64)
    end subroutine color_name_to_rgb

    !> Return hexadecimal #RGB notation for the color specified by RED GREEN
    !> BLUE.  RED, GREEN, and BLUE should be numbers between 0.0 and 1.0,
    !> inclusive.  Optional argument DIGITS_PER_COMPONENT can be either 4
    !> (the default) or 2; use the latter if you need a 24-bit
    !> specification of a color.  Port of color.el:color-rgb-to-hex.
    function color_rgb_to_hex(red, green, blue, digits_per_component) result(hex)
        real(real64), intent(in) :: red, green, blue
        integer, intent(in), optional :: digits_per_component
        character(len=:), allocatable :: hex
        character(len=13) :: buf
        integer :: digits, max_value

        digits = 4
        if (present(digits_per_component)) digits = digits_per_component
        max_value = 65535
        if (digits == 2) max_value = 255

        ! (format's %x truncates a float.)
        if (digits == 2) then
            write (buf, '("#",3z2.2)') int(red*max_value), int(green*max_value), int(blue*max_value)
            hex = to_lower(buf(1:7))
        else
            write (buf, '("#",3z4.4)') int(red*max_value), int(green*max_value), int(blue*max_value)
            hex = to_lower(buf(1:13))
        end if
    end function color_rgb_to_hex

    !> Compute hue from V1 and V2 H.
    !> Used internally by `color_hsl_to_rgb'.  Port of
    !> color.el:color-hue-to-rgb.
    real(real64) function color_hue_to_rgb(v1, v2, h)
        real(real64), intent(in) :: v1, v2, h

        if (h < 1/6.0_real64) then
            color_hue_to_rgb = v1 + (v2 - v1)*h*6.0_real64
        else if (h < 0.5_real64) then
            color_hue_to_rgb = v2
        else if (h < 2/3.0_real64) then
            color_hue_to_rgb = v1 + (v2 - v1)*(2/3.0_real64 - h)*6.0_real64
        else
            color_hue_to_rgb = v1
        end if
    end function color_hue_to_rgb

    !> Convert hue, saturation and luminance to their RGB representation.
    !> H, S, and L should each be numbers between 0.0 and 1.0, inclusive.
    !> Return an array (RED GREEN BLUE), where each element is between 0.0
    !> and 1.0, inclusive.  Port of color.el:color-hsl-to-rgb.
    function color_hsl_to_rgb(h, s, l) result(rgb)
        real(real64), intent(in) :: h, s, l
        real(real64) :: rgb(3)
        real(real64) :: m1, m2

        if (s == 0.0_real64) then
            rgb = l
            return
        end if

        if (l <= 0.5_real64) then
            m2 = l*(1.0_real64 + s)
        else
            m2 = l + s - l*s
        end if

        m1 = 2.0_real64*l - m2
        rgb(1) = color_hue_to_rgb(m1, m2, modulo(h + 1/3.0_real64, 1.0_real64))
        rgb(2) = color_hue_to_rgb(m1, m2, h)
        rgb(3) = color_hue_to_rgb(m1, m2, modulo(h - 1/3.0_real64, 1.0_real64))
    end function color_hsl_to_rgb

    ! S with its ASCII letters in lower case.
    pure function to_lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i

        out = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") out(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function to_lower
end module color_fm
