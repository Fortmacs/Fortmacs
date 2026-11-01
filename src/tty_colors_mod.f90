! tty_colors_mod.f90 -- color support for character terminals.
! Ported from GNU Emacs 31.1 lisp/term/tty-colors.el (and the color
! specification parser of src/xfaces.c).
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> An emulation of the X color functionality for character terminals.
!> The terminal's colors are kept in `tty_defined_colors', which
!> associates the colors the terminal supports with the small integers
!> (indices) sent to the terminal to switch them on.  A color is looked up
!> there first; if it is not found, its X RGB values (from
!> `color_name_rgbs', tty_colors_rgb_mod) are used to find the
!> supported color nearest to it in RGB space.
!>
!> The list is set up at startup by `tty_register_colors': the 8 standard
!> colors (`tty_register_default_colors'), then, for xterm and terminals
!> that use its setup, the 8/16/88/256 or 24-bit palette
!> (term_xterm_mod:xterm_register_default_colors).  There is one terminal,
!> so the list is one module-level registry, and display-color-cells is
!> `tty_display_color_cells', the color count it was registered for.
!>
!> RGB values are 16-bit (0..65535); a color registered without RGB values
!> reports -1 for each and is never used as an approximation.
!>
!> Deferred: `tty-color-mode-alist' and the tty-color-mode frame parameter
!> (term.c tty_setup_colors); the FRAME/DISPLAY arguments (one terminal).
module tty_colors_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint, ieee_value, ieee_quiet_nan
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use casefiddle_mod, only: downcase_string
    use tty_colors_rgb_mod, only: color_name_count, color_names, color_rgbs
    implicit none(type, external)
    private

    public :: tty_color_type, tty_colors, tty_modify_colors, tty_register_default_colors, &
              tty_color_canonicalize, tty_color_24bit, tty_color_define, tty_color_clear, &
              tty_color_off_gray_diag, tty_color_approximate, tty_color_standard_values, &
              tty_color_translate, tty_color_by_index, tty_color_values, tty_color_desc, &
              tty_color_gray_shades, color_values_from_color_spec, tty_color_assoc, tty_register_colors, &
              set_tty_display_color_cells

    !> A terminal color: its NAME, the INDEX sent to the terminal to
    !> switch it on, and, if `has_rgb', the intensities of its red, green
    !> and blue components (0..65535).  An element of `tty_colors'.
    type :: tty_color_type
        character(len=:), allocatable :: name
        integer :: index = 0
        logical :: has_rgb = .false.
        integer :: rgb(3) = -1
    end type tty_color_type

    !> The 8 standard tty colors, their indices and RGB values.  Port of
    !> term/tty-colors.el:tty-standard-colors.
    character(len=7), parameter, public :: tty_standard_color_names(8) = &
                                           [character(len=7) :: "black", "red", "green", "yellow", "blue", "magenta", &
                                                                 "cyan", "white"]
    !> The indices of `tty_standard_color_names'.
    integer, parameter, public :: tty_standard_color_indices(8) = [0, 1, 2, 3, 4, 5, 6, 7]
    !> The RGB values of `tty_standard_color_names'.
    integer, parameter, public :: tty_standard_color_rgbs(3, 8) = reshape([ &
                                                                          0, 0, 0, 65535, 0, 0, 0, 65535, 0, &
                                                                          65535, 65535, 0, 0, 0, 65535, &
                                                                          65535, 0, 65535, 0, 65535, 65535, &
                                                                          65535, 65535, 65535], [3, 8])

    !> A table of defined terminal colors and their RGB values.
    !> See the docstring of `tty_colors' for the details.  Port of
    !> xfaces.c:tty-defined-color-alist; the first
    !> `tty_defined_color_count' elements are in use.
    type(tty_color_type), allocatable :: tty_defined_colors(:)
    !> The number of colors in `tty_defined_colors'.
    integer, protected, public :: tty_defined_color_count = 0
    !> The number of colors the terminal supports, as `display-color-cells'
    !> reports it (term.c TN_max_colors): 16777216 for a 24-bit terminal.
    !> Set by `tty_register_colors'.
    integer, protected, public :: tty_display_color_cells = 0

    !> Called where upstream calls `clear-face-cache' after the color
    !> mappings change (realized faces would use stale colors).  Installed
    !> by the face code; nothing when absent.
    procedure(clear_face_cache_procedure), pointer, public :: clear_face_cache_hook => null()

    abstract interface
        !> The type of `clear_face_cache_hook'.  Not a port.
        subroutine clear_face_cache_procedure()
            implicit none(type, external)
        end subroutine clear_face_cache_procedure
    end interface

    interface
        !> Register the colors of a terminal with `ncolors' colors (its
        !> display-color-cells; 16777216 for 24-bit color): record the count
        !> in `tty_display_color_cells', register the 8 default colors as
        !> startup.el does (`tty_register_default_colors'), and if `xterm',
        !> replace them by xterm's palette for that count as
        !> term/xterm.el:terminal-init-xterm does
        !> (term_xterm_mod:xterm_register_default_colors with
        !> `xterm_standard_colors').  `status' gets xterm's error for an
        !> unsupported count.  Port of the color part of startup.el:command-line
        !> and faces.el:tty-run-terminal-initialization.
        module subroutine tty_register_colors(ncolors, xterm, status)
            implicit none(type, external)
            integer, intent(in) :: ncolors
            logical, intent(in) :: xterm
            type(status_type), intent(out) :: status
        end subroutine tty_register_colors
    end interface

contains

    !> Record that the terminal supports `ncolors' colors (display-color-cells),
    !> without registering any.  `tty_register_colors' does this too.
    !> Not a port.
    subroutine set_tty_display_color_cells(ncolors)
        integer, intent(in) :: ncolors

        tty_display_color_cells = ncolors
    end subroutine set_tty_display_color_cells

    !> Whether `a' and `b' are the same string (Fortran's `==' ignores
    !> trailing blanks).  Not a port.
    pure logical function same_string(a, b)
        character(len=*), intent(in) :: a, b

        same_string = .false.
        if (len(a) == len(b)) same_string = a == b
    end function same_string

    !> The position of `name' in `tty_defined_colors', 0 if absent
    !> (`assoc').  Not a port.
    integer function table_position(name) result(pos)
        character(len=*), intent(in) :: name
        integer :: i

        pos = 0

        do i = 1, tty_defined_color_count
            if (same_string(tty_defined_colors(i)%name, name)) then
                pos = i
                return
            end if
        end do
    end function table_position

    !> Return a table of colors supported by the terminal.
    !> Each element of the returned table has a NAME, the name of the color;
    !> an INDEX, the index of this color to be sent to the terminal driver
    !> when the color should be displayed (typically a small integer); and
    !> R, G and B, the intensities of, accordingly, red, green, and blue
    !> components of the color, represented as numbers between 0 and 65535.
    !> The file `etc/rgb.txt' in the Emacs distribution lists the standard
    !> RGB values of the X colors.  If RGB is absent (`has_rgb' false), this
    !> color will not be considered by `tty_color_translate' as an
    !> approximation to another color.  Port of
    !> term/tty-colors.el:tty-color-alist.
    function tty_colors() result(colors)
        type(tty_color_type), allocatable :: colors(:)
        integer :: i

        allocate (colors(tty_defined_color_count))

        do i = 1, tty_defined_color_count
            colors(i) = tty_defined_colors(i)
        end do
    end function tty_colors

    !> Put the association NAME, INDEX, RGB into the table of terminal colors.
    !> See `tty_colors' for details.  If the association for NAME
    !> already exists in the color table, it is modified to specify INDEX
    !> and RGB (absent `rgb': no RGB values).  Otherwise, it is appended to
    !> the end of the color table.  Port of
    !> term/tty-colors.el:tty-modify-color-alist.
    subroutine tty_modify_colors(name, index, rgb)
        character(len=*), intent(in) :: name
        integer, intent(in) :: index
        integer, intent(in), optional :: rgb(3)
        type(tty_color_type), allocatable :: grown(:)
        integer :: pos, i

        pos = table_position(name)

        if (pos == 0) then
            ! Keep the colors in the order they are registered.
            if (.not. allocated(tty_defined_colors)) allocate (tty_defined_colors(16))

            if (tty_defined_color_count == size(tty_defined_colors)) then
                allocate (grown(2*size(tty_defined_colors)))

                do i = 1, tty_defined_color_count
                    call move_alloc(tty_defined_colors(i)%name, grown(i)%name)
                    grown(i)%index = tty_defined_colors(i)%index
                    grown(i)%has_rgb = tty_defined_colors(i)%has_rgb
                    grown(i)%rgb = tty_defined_colors(i)%rgb
                end do

                call move_alloc(grown, tty_defined_colors)
            end if

            tty_defined_color_count = tty_defined_color_count + 1
            pos = tty_defined_color_count
            tty_defined_colors(pos)%name = name
        end if

        tty_defined_colors(pos)%index = index
        tty_defined_colors(pos)%has_rgb = present(rgb)

        if (present(rgb)) then
            tty_defined_colors(pos)%rgb = rgb
        else
            tty_defined_colors(pos)%rgb = -1
        end if
    end subroutine tty_modify_colors

    !> Register the default set of colors for a character terminal.
    !> Port of term/tty-colors.el:tty-register-default-colors.
    subroutine tty_register_default_colors()
        type(status_type) :: status
        integer :: i

        do i = 1, size(tty_standard_color_names)
            call tty_color_define(trim(tty_standard_color_names(i)), tty_standard_color_indices(i), status, &
                                  tty_standard_color_rgbs(:, i))
        end do

        ! Modifying color mappings means realized faces don't use the
        ! right colors, so clear them.
        if (associated(clear_face_cache_hook)) call clear_face_cache_hook()
    end subroutine tty_register_default_colors

    !> Return `color' in canonical form.
    !> A canonicalized color name is all-lower case, with any blanks removed.
    !> Port of term/tty-colors.el:tty-color-canonicalize.
    function tty_color_canonicalize(color) result(canon)
        character(len=*), intent(in) :: color
        character(len=:), allocatable :: canon
        character(len=:), allocatable :: lower, buffer
        integer :: i, n
        logical :: match

        match = .false.

        do i = 1, len(color)
            if ((color(i:i) >= "A" .and. color(i:i) <= "Z") .or. color(i:i) == " ") then
                match = .true.
                exit
            end if
        end do

        if (.not. match) then
            canon = color
            return
        end if

        lower = downcase_string(color)
        allocate (character(len=len(lower)) :: buffer)
        n = 0

        do i = 1, len(lower)
            if (lower(i:i) /= " ") then
                n = n + 1
                buffer(n:n) = lower(i:i)
            end if
        end do

        canon = buffer(1:n)
    end function tty_color_canonicalize

    !> Return 24-bit color pixel value for RGB value `rgb'.
    !> If the terminal is not a 24-bit TTY terminal, return -1.  Port of
    !> term/tty-colors.el:tty-color-24bit.
    integer function tty_color_24bit(rgb) result(pixel)
        integer, intent(in) :: rgb(3)

        pixel = -1

        if (tty_display_color_cells == 16777216) then
            pixel = ior(ior(ishft(shifta(rgb(1), 8), 16), ishft(shifta(rgb(2), 8), 8)), shifta(rgb(3), 8))
        end if
    end function tty_color_24bit

    !> Specify a tty color by its NAME, terminal INDEX and RGB values.
    !> NAME is a string, INDEX is typically a small integer used to send to
    !> the terminal driver a command to switch this color on, and RGB is an
    !> array of 3 numbers that specify the intensity of red, green, and blue
    !> components of the color.
    !> If specified, each one of the RGB components must be a number between
    !> 0 and 65535.  If RGB is omitted, the specified color will never be used
    !> by `tty_color_translate' as an approximation to another color.  On a
    !> 24-bit terminal the color's pixel value replaces INDEX.  Port of
    !> term/tty-colors.el:tty-color-define.
    subroutine tty_color_define(name, index, status, rgb)
        character(len=*), intent(in) :: name
        integer, intent(in) :: index
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: rgb(:)
        integer :: pixel

        if (present(rgb)) then
            if (size(rgb) /= 3) then
                call set_error(status, FM_GENERIC_ERROR, 'Invalid specification for tty color "'//name//'"')
                return
            end if

            pixel = tty_color_24bit(rgb)
            if (pixel < 0) pixel = index
            call tty_modify_colors(tty_color_canonicalize(name), pixel, rgb)
        else
            call tty_modify_colors(tty_color_canonicalize(name), index)
        end if
    end subroutine tty_color_define

    !> Clear the list of supported tty colors.  Port of
    !> term/tty-colors.el:tty-color-clear.
    subroutine tty_color_clear()
        if (allocated(tty_defined_colors)) deallocate (tty_defined_colors)
        tty_defined_color_count = 0
    end subroutine tty_color_clear

    !> Compute the angle between the color given by R,G,B and the gray diagonal.
    !> The gray diagonal is the diagonal of the 3D cube in RGB space which
    !> connects the points corresponding to the black and white colors.  All the
    !> colors whose RGB coordinates belong to this diagonal are various shades
    !> of gray, thus the name.  The result is a NaN when rounding pushes the
    !> cosine above 1, as upstream's `acos'.  Port of
    !> term/tty-colors.el:tty-color-off-gray-diag.
    real(real64) function tty_color_off_gray_diag(r, g, b) result(angle)
        integer, intent(in) :: r, g, b
        real(real64) :: mag, cosine

        mag = sqrt(real(3*(r*r + g*g + b*b), real64))

        if (mag < 1) then
            angle = 0
        else
            cosine = real(r + g + b, real64)/mag

            if (abs(cosine) > 1) then
                ! acos's domain error: a NaN.
                angle = ieee_value(angle, ieee_quiet_nan)
            else
                angle = acos(cosine)
            end if
        end if
    end function tty_color_off_gray_diag

    !> The position in `tty_defined_colors' of the color that best
    !> approximates `rgb', 0 if none has RGB values.  Not a port.
    integer function approximate_position(rgb) result(best)
        integer, intent(in) :: rgb(3)
        integer :: best_distance, r, g, b, try_r, try_g, try_b, dif_r, dif_g, dif_b, dist, i
        logical :: favor_non_gray

        best = 0
        best_distance = 195076 ! 3 * 255^2 + 15
        r = shifta(rgb(1), 8)
        g = shifta(rgb(2), 8)
        b = shifta(rgb(3), 8)
        ! If the approximated color is not close enough to the gray
        ! diagonal of the RGB cube, favor non-gray colors.  (The number
        ! 0.065 is an empirical ad-hoc'ery.)  A NaN angle compares false.
        favor_non_gray = tty_color_off_gray_diag(r, g, b) >= 0.065_real64

        do i = 1, tty_defined_color_count
            ! If the RGB values of the candidate color are unknown, we
            ! never consider it for approximating another color.
            if (.not. tty_defined_colors(i)%has_rgb) cycle
            try_r = shifta(tty_defined_colors(i)%rgb(1), 8)
            try_g = shifta(tty_defined_colors(i)%rgb(2), 8)
            try_b = shifta(tty_defined_colors(i)%rgb(3), 8)
            dif_r = r - try_r
            dif_g = g - try_g
            dif_b = b - try_b
            dist = dif_r*dif_r + dif_g*dif_g + dif_b*dif_b

            ! The candidate color is on the gray diagonal if its RGB
            ! components are all equal.
            if (dist < best_distance .and. (try_r /= try_g .or. try_g /= try_b .or. .not. favor_non_gray)) then
                best_distance = dist
                best = i
            end if
        end do
    end function approximate_position

    !> Copy element `pos' of `tty_defined_colors' to the outputs.
    !> Not a port.
    subroutine describe(pos, index, r, g, b, name, has_rgb)
        integer, intent(in) :: pos
        integer, intent(out) :: index, r, g, b
        character(len=:), allocatable, intent(out), optional :: name
        logical, intent(out), optional :: has_rgb

        index = tty_defined_colors(pos)%index
        r = tty_defined_colors(pos)%rgb(1)
        g = tty_defined_colors(pos)%rgb(2)
        b = tty_defined_colors(pos)%rgb(3)
        if (present(name)) name = tty_defined_colors(pos)%name
        if (present(has_rgb)) has_rgb = tty_defined_colors(pos)%has_rgb
    end subroutine describe

    !> Find the color in `tty_colors' that best approximates RGB.
    !> The value is its NAME, INDEX and R, G, B; `found' is false when no
    !> color qualifies.
    !> The argument RGB should be an rgb value, that is, an array of three
    !> integers in the 0..65535 range.  Port of
    !> term/tty-colors.el:tty-color-approximate.
    subroutine tty_color_approximate(rgb, found, name, index, r, g, b)
        integer, intent(in) :: rgb(3)
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out) :: name
        integer, intent(out) :: index, r, g, b
        integer :: pos

        pos = approximate_position(rgb)
        found = pos > 0
        index = -1
        r = -1
        g = -1
        b = -1
        if (found) call describe(pos, index, r, g, b, name)
    end subroutine tty_color_approximate

    !> Parse hex color component specification `s'.  Set `dst' to the
    !> parsed value normalized so that the maximum value for the number of
    !> hex digits given becomes 65535, and return true on success, false
    !> otherwise.  Port of xfaces.c:parse_hex_color_comp.
    logical function parse_hex_color_comp(s, dst) result(ok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: dst
        integer(int64) :: val, maxval
        integer :: n, i, digit

        ok = .false.
        n = len(s)
        if (n <= 0 .or. n > 4) return
        val = 0

        do i = 1, n
            select case (s(i:i))
            case ("0":"9")
                digit = iachar(s(i:i)) - iachar("0")
            case ("A":"F")
                digit = iachar(s(i:i)) - iachar("A") + 10
            case ("a":"f")
                digit = iachar(s(i:i)) - iachar("a") + 10
            case default
                return
            end select

            val = ior(ishft(val, 4), int(digit, int64))
        end do

        maxval = ishft(1_int64, n*4) - 1
        dst = int(val*65535/maxval)
        ok = .true.
    end function parse_hex_color_comp

    !> Whether `s' is a decimal floating-point number as C's strtod reads
    !> it entirely: an optional sign, digits with an optional point (at
    !> least one digit), and an optional exponent with at least one digit.
    !> Not a port.
    logical function is_strtod_decimal(s) result(ok)
        character(len=*), intent(in) :: s
        integer :: i, n, digits

        ok = .false.
        n = len(s)
        i = 1

        if (i <= n) then
            if (s(i:i) == "+" .or. s(i:i) == "-") i = i + 1
        end if

        digits = 0

        do while (i <= n)
            if (s(i:i) < "0" .or. s(i:i) > "9") exit
            digits = digits + 1
            i = i + 1
        end do

        if (i <= n) then
            if (s(i:i) == ".") then
                i = i + 1

                do while (i <= n)
                    if (s(i:i) < "0" .or. s(i:i) > "9") exit
                    digits = digits + 1
                    i = i + 1
                end do
            end if
        end if

        if (digits == 0) return

        if (i <= n) then
            if (s(i:i) /= "e" .and. s(i:i) /= "E") return
            i = i + 1

            if (i <= n) then
                if (s(i:i) == "+" .or. s(i:i) == "-") i = i + 1
            end if

            digits = 0

            do while (i <= n)
                if (s(i:i) < "0" .or. s(i:i) > "9") exit
                digits = digits + 1
                i = i + 1
            end do

            if (digits == 0) return
        end if

        ok = i > n
    end function is_strtod_decimal

    !> Parse floating-point color component specification `s'.  Put the
    !> integer near-equivalent of that into `dst'.  Return true if
    !> successful, false otherwise.  An empty `s' reads as 0, as strtod
    !> converting nothing ends where the component ends.  Port of
    !> xfaces.c:parse_float_color_comp.
    logical function parse_float_color_comp(s, dst) result(ok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: dst
        real(real64) :: x
        integer :: i, ios

        ok = .false.

        ! Only allow decimal float literals without whitespace.
        do i = 1, len(s)
            if (.not. ((s(i:i) >= "0" .and. s(i:i) <= "9") .or. s(i:i) == "." .or. s(i:i) == "+" &
                       .or. s(i:i) == "-" .or. s(i:i) == "e" .or. s(i:i) == "E")) return
        end do

        if (len(s) == 0) then
            x = 0
        else
            if (.not. is_strtod_decimal(s)) return
            read (s, *, iostat=ios) x
            if (ios /= 0) return
        end if

        if (0 <= x .and. x <= 1) then
            dst = nint(ieee_rint(x*65535))
            ok = .true.
        end if
    end function parse_float_color_comp

    !> Parse color SPEC as a numeric color and return RED, GREEN and BLUE.
    !> This function recognizes the following formats for SPEC:
    !>
    !>  #RGB, where R, G and B are hex numbers of equal length, 1-4 digits each.
    !>  rgb:R/G/B, where R, G, and B are hex numbers, 1-4 digits each.
    !>  rgbi:R/G/B, where R, G and B are floating-point numbers in [0,1].
    !>
    !> If SPEC is not in one of the above forms, `found' is false.
    !>
    !> Each of the 3 integers of the result, RED, GREEN, and BLUE,
    !> is normalized to have its value in [0,65535].  Port of
    !> xfaces.c:color-values-from-color-spec (parse_color_spec).
    subroutine color_values_from_color_spec(spec, found, r, g, b)
        character(len=*), intent(in) :: spec
        logical, intent(out) :: found
        integer, intent(out) :: r, g, b
        integer :: n, sep1, sep2, start

        found = .false.
        r = -1
        g = -1
        b = -1
        if (len(spec) == 0) return

        if (spec(1:1) == "#") then
            if (mod(len(spec) - 1, 3) == 0) then
                n = (len(spec) - 1)/3
                found = parse_hex_color_comp(spec(2:1 + n), r)
                if (found) found = parse_hex_color_comp(spec(2 + n:1 + 2*n), g)
                if (found) found = parse_hex_color_comp(spec(2 + 2*n:1 + 3*n), b)
            end if
        else if (index(spec, "rgb:") == 1 .or. index(spec, "rgbi:") == 1) then
            start = 5
            if (spec(4:4) == "i") start = 6
            sep1 = index(spec(start:), "/")
            if (sep1 == 0) return
            sep1 = sep1 + start - 1
            sep2 = index(spec(sep1 + 1:), "/")
            if (sep2 == 0) return
            sep2 = sep2 + sep1

            if (start == 5) then
                found = parse_hex_color_comp(spec(start:sep1 - 1), r)
                if (found) found = parse_hex_color_comp(spec(sep1 + 1:sep2 - 1), g)
                if (found) found = parse_hex_color_comp(spec(sep2 + 1:), b)
            else
                found = parse_float_color_comp(spec(start:sep1 - 1), r)
                if (found) found = parse_float_color_comp(spec(sep1 + 1:sep2 - 1), g)
                if (found) found = parse_float_color_comp(spec(sep2 + 1:), b)
            end if
        end if

        if (.not. found) then
            r = -1
            g = -1
            b = -1
        end if
    end subroutine color_values_from_color_spec

    !> Return standard RGB values of the color COLOR.
    !>
    !> The result is RED, GREEN and BLUE, integers that range from 0 to
    !> 65535; white is 65535 65535 65535.  `found' is false when COLOR is
    !> neither a numeric color specification nor a known color name.
    !>
    !> The returned value reflects the standard Emacs definition of
    !> COLOR (see the info node `(emacs) Colors'), regardless of whether
    !> the terminal can display it, so the return value should be the
    !> same regardless of what display is being used.  Port of
    !> term/tty-colors.el:tty-color-standard-values.
    subroutine tty_color_standard_values(color, found, r, g, b)
        character(len=*), intent(in) :: color
        logical, intent(out) :: found
        integer, intent(out) :: r, g, b
        integer :: i

        call color_values_from_color_spec(color, found, r, g, b)
        if (found) return

        do i = 1, color_name_count
            if (same_string(trim(color_names(i)), color)) then
                found = .true.
                r = color_rgbs(1, i)
                g = color_rgbs(2, i)
                b = color_rgbs(3, i)
                return
            end if
        end do
    end subroutine tty_color_standard_values

    !> Look COLOR up in `tty_colors' as it is (no canonicalization):
    !> `found' tells whether it is there, with its INDEX and R, G, B (-1
    !> each without RGB values).  The `assoc' xfaces.c:map_tty_color does.
    !> Not a port.
    subroutine tty_color_assoc(color, found, index, r, g, b)
        character(len=*), intent(in) :: color
        logical, intent(out) :: found
        integer, intent(out) :: index, r, g, b
        integer :: pos

        pos = table_position(color)
        found = pos > 0
        index = -1
        r = -1
        g = -1
        b = -1
        if (found) call describe(pos, index, r, g, b)
    end subroutine tty_color_assoc

    !> Given a color COLOR, return the index of the corresponding TTY color.
    !>
    !> COLOR must be a string that is either the color's name, or its
    !> color triplet specification like "#RRGGBB" or "rgb:RR/GG/BB",
    !> where each primary color can be given with 1 to 4 hex digits.
    !>
    !> If COLOR is a color name that is found among supported colors in
    !> `tty_colors', the associated index is returned.  Otherwise, the
    !> RGB values of the color, either as given by the argument or from
    !> looking up the name in `color_name_rgbs', are used to find the
    !> supported color that is the best approximation for COLOR in the RGB
    !> space.
    !> If COLOR is neither a valid RGB specification of the color, nor a
    !> name of a color in `color_name_rgbs', the returned value is -1.
    !> Port of term/tty-colors.el:tty-color-translate.
    integer function tty_color_translate(color) result(index)
        character(len=*), intent(in) :: color
        integer :: r, g, b
        logical :: found

        call tty_color_desc(color, found, index, r, g, b)
        if (.not. found) index = -1
    end function tty_color_translate

    !> Given a numeric index of a tty color, return its description:
    !> its NAME and, optionally, R, G, B (-1 each without RGB values).
    !> `found' is false when no color has that index; when several have it,
    !> the last one registered wins.  Port of
    !> term/tty-colors.el:tty-color-by-index.
    subroutine tty_color_by_index(index, name, found, r, g, b)
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        integer, intent(out), optional :: r, g, b
        integer :: i, pos, color_index, rr, gg, bb

        pos = 0

        do i = 1, tty_defined_color_count
            if (tty_defined_colors(i)%index == index) pos = i
        end do

        found = pos > 0
        rr = -1
        gg = -1
        bb = -1
        if (found) call describe(pos, color_index, rr, gg, bb, name)
        if (present(r)) r = rr
        if (present(g)) g = gg
        if (present(b)) b = bb
    end subroutine tty_color_by_index

    !> Return RGB values of the color COLOR on the terminal.
    !>
    !> If COLOR is not directly supported by the display, return the RGB
    !> values for a supported color that is its best approximation.
    !> The value is R, G and B, integers that range from 0 to 65535; white
    !> is 65535 65535 65535.  `found' is false when COLOR has no description
    !> or its description has no RGB values.  Port of
    !> term/tty-colors.el:tty-color-values.
    subroutine tty_color_values(color, found, r, g, b)
        character(len=*), intent(in) :: color
        logical, intent(out) :: found
        integer, intent(out) :: r, g, b
        integer :: index
        logical :: has_rgb

        call tty_color_desc(color, found, index, r, g, b, has_rgb=has_rgb)
        found = found .and. has_rgb
    end subroutine tty_color_values

    !> Return the description of the color COLOR for a character terminal.
    !> The value is its INDEX and R, G, B (-1 each, and `has_rgb' false,
    !> when the color has no RGB values), and optionally its NAME.  The
    !> returned NAME or RGB value may not be the same as the argument COLOR,
    !> because the latter might need to be approximated if it is not
    !> supported directly.  `found' is false when COLOR is neither defined,
    !> nor a color specification or known name that some defined color
    !> approximates.  Port of term/tty-colors.el:tty-color-desc.
    subroutine tty_color_desc(color, found, index, r, g, b, name, has_rgb)
        character(len=*), intent(in) :: color
        logical, intent(out) :: found
        integer, intent(out) :: index, r, g, b
        character(len=:), allocatable, intent(out), optional :: name
        logical, intent(out), optional :: has_rgb
        character(len=:), allocatable :: canon
        integer :: pos, pixel, rgb(3)

        canon = tty_color_canonicalize(color)
        index = -1
        r = -1
        g = -1
        b = -1
        if (present(has_rgb)) has_rgb = .false.
        pos = table_position(canon)

        if (pos == 0) then
            call tty_color_standard_values(canon, found, rgb(1), rgb(2), rgb(3))
            if (.not. found) return
            pixel = tty_color_24bit(rgb)

            if (pixel >= 0) then
                index = pixel
                r = rgb(1)
                g = rgb(2)
                b = rgb(3)
                if (present(name)) name = canon
                if (present(has_rgb)) has_rgb = .true.
                return
            end if

            pos = approximate_position(rgb)
        end if

        found = pos > 0
        if (found) call describe(pos, index, r, g, b, name, has_rgb)
    end subroutine tty_color_desc

    !> Return the number of gray colors supported by the terminal.
    !> A color is considered gray if the 3 components of its RGB value are
    !> equal.  Port of term/tty-colors.el:tty-color-gray-shades.
    integer function tty_color_gray_shades() result(count)
        integer :: i

        count = 0

        do i = 1, tty_defined_color_count
            if (.not. tty_defined_colors(i)%has_rgb) cycle
            if (tty_defined_colors(i)%rgb(1) == tty_defined_colors(i)%rgb(2) .and. &
                tty_defined_colors(i)%rgb(2) == tty_defined_colors(i)%rgb(3)) count = count + 1
        end do
    end function tty_color_gray_shades
end module tty_colors_mod
