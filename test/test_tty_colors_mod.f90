! test_tty_colors_mod.f90 -- tests for tty_colors_mod (and the xterm
! palettes of term_xterm_mod): terminal colors.
! Ports GNU Emacs 31.1 test/lisp/term/tty-colors-tests.el in part; only
! tty-colors-test-standard-colors.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

module test_tty_colors_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use tty_colors_mod, only: tty_register_colors, tty_color_translate, tty_color_desc, tty_color_by_index, &
                              tty_color_standard_values, tty_color_canonicalize, tty_color_define, tty_color_values, &
                              tty_color_gray_shades, tty_defined_color_count, color_values_from_color_spec, &
                              tty_color_approximate, tty_color_clear, tty_color_assoc
    implicit none(type, external)
    private

    public :: collect_tty_colors_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_tty_colors_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("tty-colors-test-standard-colors", test_standard_colors), &
                    new_unittest("color-values-from-color-spec", test_color_spec), &
                    new_unittest("tty-color-canonicalize", test_canonicalize), &
                    new_unittest("default-8-colors", test_default_colors), &
                    new_unittest("xterm-8-colors", test_xterm_8), &
                    new_unittest("xterm-88-colors", test_xterm_88), &
                    new_unittest("xterm-256-colors", test_xterm_256), &
                    new_unittest("xterm-24bit-colors", test_xterm_24bit), &
                    new_unittest("xterm-unsupported-colors", test_xterm_unsupported), &
                    new_unittest("tty-color-define-without-rgb", test_define_without_rgb) &
                    ]
    end subroutine collect_tty_colors_mod

    !> Check that `color' has the standard values `rgb'.
    subroutine check_standard(error, color, rgb)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: color
        integer, intent(in) :: rgb(3)
        integer :: r, g, b
        logical :: found

        call tty_color_standard_values(color, found, r, g, b)
        call check(error, found, color//" has standard values")
        if (allocated(error)) return
        call check(error, all([r, g, b] == rgb), color//" standard values")
    end subroutine check_standard

    !> Port of tty-colors-tests.el:tty-colors-test-standard-colors.
    subroutine test_standard_colors(error)
        type(error_type), allocatable, intent(out) :: error

        call check_standard(error, "white", [65535, 65535, 65535])
        if (allocated(error)) return
        call check_standard(error, "#F00", [65535, 0, 0])
        if (allocated(error)) return
        call check_standard(error, "#00FF00", [0, 65535, 0])
        if (allocated(error)) return
        call check_standard(error, "#00000000FFFF", [0, 0, 65535])
        if (allocated(error)) return
        call check_standard(error, "rgb:0/0/7", [0, 0, 30583])
        if (allocated(error)) return
        call check_standard(error, "rgb:0/ff/0", [0, 65535, 0])
        if (allocated(error)) return
        call check_standard(error, "rgb:ffFF/0000/0000", [65535, 0, 0])
    end subroutine test_standard_colors

    !> The numeric forms, and what is not one.
    subroutine test_color_spec(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: r, g, b
        logical :: found

        call check_standard(error, "#123", [4369, 8738, 13107])
        if (allocated(error)) return
        ! lrint rounds 32767.5 to even.
        call check_standard(error, "rgbi:1/0/0.5", [65535, 0, 32768])
        if (allocated(error)) return
        call check_standard(error, "rgbi:1e-0/.25/+0.", [65535, 16384, 0])
        if (allocated(error)) return
        ! strtod converting nothing at a separator reads 0.
        call check_standard(error, "rgbi://", [0, 0, 0])
        if (allocated(error)) return
        call check_standard(error, "gray75", [49087, 49087, 49087])
        if (allocated(error)) return
        call color_values_from_color_spec("#1234", found, r, g, b)
        call check(error,.not. found, "#1234 has 4 digits")
        if (allocated(error)) return
        call color_values_from_color_spec("#123451234512345", found, r, g, b)
        call check(error,.not. found, "more than 4 digits per component")
        if (allocated(error)) return
        call color_values_from_color_spec("rgb:1/2", found, r, g, b)
        call check(error,.not. found, "rgb: needs three components")
        if (allocated(error)) return
        call color_values_from_color_spec("rgb:1//2", found, r, g, b)
        call check(error,.not. found, "an empty hex component")
        if (allocated(error)) return
        call color_values_from_color_spec("rgbi:1.5/0/0", found, r, g, b)
        call check(error,.not. found, "rgbi: above 1")
        if (allocated(error)) return
        call color_values_from_color_spec("rgbi:1e/0/0", found, r, g, b)
        call check(error,.not. found, "rgbi: exponent without digits")
        if (allocated(error)) return
        ! Names are not canonicalized here.
        call tty_color_standard_values("Gray75", found, r, g, b)
        call check(error,.not. found, "Gray75 is not a name")
        if (allocated(error)) return
        call tty_color_standard_values("gray75 ", found, r, g, b)
        call check(error,.not. found, "a trailing blank")
    end subroutine test_color_spec

    subroutine test_canonicalize(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, tty_color_canonicalize("Red"), "red")
        if (allocated(error)) return
        call check(error, tty_color_canonicalize("Light  Goldenrod Yellow"), "lightgoldenrodyellow")
        if (allocated(error)) return
        call check(error, tty_color_canonicalize("grey"), "grey")
        if (allocated(error)) return
        call check(error, tty_color_canonicalize("#FF0000"), "#ff0000")
    end subroutine test_canonicalize

    !> The 8 colors startup.el registers.  "light green" is nearest to
    !> yellow (cyan is as near, but later).
    subroutine test_default_colors(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found
        character(len=:), allocatable :: name

        call tty_color_clear()
        call tty_register_colors(8, .false., status)
        call check(error, status%ok() .and. tty_defined_color_count == 8, "8 colors")
        if (allocated(error)) return
        call check(error, tty_color_translate("red"), 1)
        if (allocated(error)) return
        call check(error, tty_color_translate("Red"), 1)
        if (allocated(error)) return
        call check(error, tty_color_translate("#ff0000"), 1)
        if (allocated(error)) return
        ! Not gray: black would be nearer than red by distance.
        call check(error, tty_color_translate("darkred"), 1)
        if (allocated(error)) return
        call check(error, tty_color_translate("gray75"), 7)
        if (allocated(error)) return
        call check(error, tty_color_translate("grey75"), 7)
        if (allocated(error)) return
        call check(error, tty_color_translate("nosuchcolor"), -1)
        if (allocated(error)) return
        call tty_color_desc("Dark Red", found, index, r, g, b, name)
        call check(error, found .and. index == 1 .and. name == "red" .and. r == 65535 .and. g == 0 .and. b == 0, &
                   "desc of Dark Red")
        if (allocated(error)) return
        call tty_color_values("light green", found, r, g, b)
        call check(error, found .and. r == 65535 .and. g == 65535 .and. b == 0, "values of light green")
        if (allocated(error)) return
        call tty_color_by_index(6, name, found)
        call check(error, found .and. name == "cyan", "color 6")
        if (allocated(error)) return
        call tty_color_by_index(8, name, found)
        call check(error,.not. found, "no color 8")
        if (allocated(error)) return
        call tty_color_approximate([0, 0, 30000], found, name, index, r, g, b)
        call check(error, found .and. name == "blue", "approximate dark blue")
        if (allocated(error)) return
        call check(error, tty_color_gray_shades(), 2)
    end subroutine test_default_colors

    !> xterm's 8 colors replace the defaults.
    subroutine test_xterm_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found

        call tty_register_colors(8, .true., status)
        call check(error, status%ok() .and. tty_defined_color_count == 8, "8 colors")
        if (allocated(error)) return
        call tty_color_desc("red", found, index, r, g, b)
        call check(error, found .and. index == 1 .and. r == 52685 .and. g == 0 .and. b == 0, "xterm's red3")
        if (allocated(error)) return
        call tty_color_assoc("brightred", found, index, r, g, b)
        call check(error,.not. found, "no bright colors")
        if (allocated(error)) return
        call check(error, tty_color_translate("#ff0000"), 1)
    end subroutine test_xterm_8

    subroutine test_xterm_88(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found
        character(len=:), allocatable :: name

        call tty_register_colors(88, .true., status)
        call check(error, status%ok() .and. tty_defined_color_count == 88, "88 colors")
        if (allocated(error)) return
        call tty_color_desc("color-79", found, index, r, g, b)
        call check(error, found .and. index == 79 .and. all([r, g, b] == 65535), "color-79")
        if (allocated(error)) return
        call tty_color_desc("color-80", found, index, r, g, b)
        call check(error, found .and. all([r, g, b] == 46*257), "color-80")
        if (allocated(error)) return
        call tty_color_desc("color-87", found, index, r, g, b)
        call check(error, found .and. all([r, g, b] == 231*257), "color-87")
        if (allocated(error)) return
        call tty_color_desc("color-17", found, index, r, g, b)
        call check(error, found .and. r == 0 .and. g == 0 .and. b == 139*257, "color-17")
        if (allocated(error)) return
        call tty_color_by_index(85, name, found)
        call check(error, found .and. name == "color-85", "color 85")
    end subroutine test_xterm_88

    subroutine test_xterm_256(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found
        character(len=:), allocatable :: name

        call tty_register_colors(256, .true., status)
        call check(error, status%ok() .and. tty_defined_color_count == 256, "256 colors")
        if (allocated(error)) return
        call check(error, tty_color_translate("red"), 1)
        if (allocated(error)) return
        call check(error, tty_color_translate("color-196"), 196)
        if (allocated(error)) return
        call check(error, tty_color_translate("#ff0000"), 9)
        if (allocated(error)) return
        call check(error, tty_color_translate("gray75"), 250)
        if (allocated(error)) return
        call check(error, tty_color_translate("Grey 75"), 250)
        if (allocated(error)) return
        call check(error, tty_color_translate("darkred"), 88)
        if (allocated(error)) return
        call check(error, tty_color_translate("orange"), 214)
        if (allocated(error)) return
        call check(error, tty_color_translate("lightgreen"), 120)
        if (allocated(error)) return
        call check(error, tty_color_translate("brown"), 124)
        if (allocated(error)) return
        call tty_color_desc("color-16", found, index, r, g, b)
        call check(error, found .and. all([r, g, b] == 0), "color-16")
        if (allocated(error)) return
        call tty_color_desc("color-17", found, index, r, g, b)
        call check(error, found .and. r == 0 .and. g == 0 .and. b == 95*257, "color-17")
        if (allocated(error)) return
        call tty_color_desc("color-232", found, index, r, g, b)
        call check(error, found .and. all([r, g, b] == 8*257), "color-232")
        if (allocated(error)) return
        call tty_color_desc("color-255", found, index, r, g, b, name)
        call check(error, found .and. all([r, g, b] == 238*257) .and. name == "color-255", "color-255")
        if (allocated(error)) return
        call tty_color_by_index(12, name, found)
        call check(error, found .and. name == "brightblue", "color 12")
        if (allocated(error)) return
        call check(error, tty_color_gray_shades(), 34)
    end subroutine test_xterm_256

    !> On a 24-bit terminal every named color is defined, by pixel value.
    subroutine test_xterm_24bit(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found
        character(len=:), allocatable :: name

        call tty_register_colors(16777216, .true., status)
        call check(error, status%ok() .and. tty_defined_color_count == 665, "16 + 649 colors")
        if (allocated(error)) return
        call check(error, tty_color_translate("red"), 205*65536)
        if (allocated(error)) return
        call check(error, tty_color_translate("gray75"), 191*65536 + 191*256 + 191)
        if (allocated(error)) return
        call tty_color_desc("#123456", found, index, r, g, b, name)
        call check(error, found .and. index == 1193046 .and. name == "#123456" .and. r == 4626 .and. g == 13364 &
                   .and. b == 22102, "#123456 by pixel")
        if (allocated(error)) return
        call check(error, tty_color_translate("nosuchcolor"), -1)
        if (allocated(error)) return
        call tty_color_by_index(255*65536, name, found)
        call check(error, found .and. name == "red1", "the last of the equal pixels")
        if (allocated(error)) return
        ! Back to 8 colors: the indices are small again.
        call tty_register_colors(8, .false., status)
        call check(error, tty_color_translate("red"), 1)
    end subroutine test_xterm_24bit

    subroutine test_xterm_unsupported(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call tty_register_colors(52, .true., status)
        call check(error,.not. status%ok(), "52 colors fail")
        if (allocated(error)) return
        call check(error, status%detail, "Unsupported number of xterm colors (52)")
        if (allocated(error)) return
        call check(error, tty_defined_color_count, 16)
        if (allocated(error)) return
        ! No color support: xterm's colors are not cleared, the 8 defaults
        ! are redefined.
        call tty_register_colors(-1, .true., status)
        call check(error, status%ok() .and. tty_defined_color_count == 16, "no colors")
        if (allocated(error)) return
        call check(error, tty_color_translate("red"), 1)
    end subroutine test_xterm_unsupported

    !> A color without RGB values is found by name only.
    subroutine test_define_without_rgb(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: index, r, g, b
        logical :: found, has_rgb

        call tty_color_clear()
        call tty_color_define("Foo Bar", 42, status)
        call check(error, status%ok(), "defined")
        if (allocated(error)) return
        call tty_color_desc("foobar", found, index, r, g, b, has_rgb=has_rgb)
        call check(error, found .and. index == 42 .and. .not. has_rgb .and. r == -1, "foobar")
        if (allocated(error)) return
        call tty_color_values("foobar", found, r, g, b)
        call check(error,.not. found, "no values")
        if (allocated(error)) return
        call check(error, tty_color_translate("red"), -1)
        if (allocated(error)) return
        call tty_color_define("bad", 1, status, [1, 2])
        call check(error, status%detail, 'Invalid specification for tty color "bad"')
        if (allocated(error)) return
        call tty_register_colors(8, .false., status)
    end subroutine test_define_without_rgb
end module test_tty_colors_mod
