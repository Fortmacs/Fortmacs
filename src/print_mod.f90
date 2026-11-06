! print_mod.f90 -- printed representation of values.
! Ported from GNU Emacs 31.1 src/print.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> The printed representation of a value, as `prin1' writes it, and as
!> Fortran (`fortran_text'), which help shows for a variable holding a value.
!>
!> Floats (real64 values: float values are deferred) are written by
!> `float_to_string', and `sprintf_float' writes them as C's printf
!> does for `format_text'.
!>
!> Deferred: print streams, `print_length' and `print_level',
!> escaping in symbol names, circular structure, the text properties of
!> a string (#("..." START END PROPERTIES)), `float_output_format', the char-table
!> of a full keymap (made by `make_keymap'; printed "#<keymap>"), and
!> the place of character ranges among a sparse keymap's other bindings
!> (they are printed first).
module print_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    use error_mod, only: status_type
    use value_mod, only: value_type, vtag, V_NULL, V_INTEGER, V_NAME, V_STRING, V_ARRAY, V_BUFFER, V_MARKER, &
                         V_KEYMAP, xfixnum, xstring, xhandle, list_elements
    use data_mod, only: symbol_name
    use buffer_mod, only: buffer_name, buffers
    use handle_mod, only: handle_type
    use marker_mod, only: markers, resolve_marker
    use keymap_mod, only: is_keymap_live, is_keymap_full, keymap_own_prompt, keymap_parent, keymap_entries
    implicit none(type, external)
    private

    public :: readable_text, quoted_text, marker_text, fortran_text, fortran_string, float_to_string, sprintf_float

    !> C's printf with a conversion %e, %f or %g (`format_text' uses it):
    !> a real64 value, or an integer written exactly (as upstream writes
    !> one as a long double).  Not a port.
    interface sprintf_float
        module procedure sprintf_real, sprintf_integer
    end interface sprintf_float

contains

    !> Return a string containing the printed representation of OBJECT.
    !> OBJECT can be any object.  Strings are quoted, a backslash
    !> escaping each double quote and backslash in them, unless NOESCAPE
    !> is present and true: then they are written as they are, as
    !> `princ' does.  Port of print.c:Fprin1_to_string.
    recursive function readable_text(object, noescape) result(s)
        type(value_type), intent(in) :: object
        logical, intent(in), optional :: noescape
        character(len=:), allocatable :: s
        type(value_type), allocatable :: elements(:)
        type(status_type) :: status
        character(len=21) :: buffer
        character(len=:), allocatable :: text
        integer :: k
        logical :: raw

        raw = .false.
        if (present(noescape)) raw = noescape

        select case (vtag(object))
        case (V_NULL)
            s = "nil"
        case (V_INTEGER)
            write (buffer, "(i0)") xfixnum(object)
            s = trim(buffer)
        case (V_NAME)
            call symbol_name(object, s, status)
            if (.not. status%ok()) s = "#<symbol>"
        case (V_STRING)
            text = xstring(object)

            if (raw) then
                s = text
            else
                s = quoted_text(text)
            end if
        case (V_ARRAY)
            call list_elements(object, elements)
            s = "("

            do k = 1, size(elements)
                if (k > 1) s = s//" "
                s = s//readable_text(elements(k), raw)
            end do

            s = s//")"
        case (V_BUFFER)
            call buffer_name(xhandle(object), text, status)

            if (status%ok()) then
                s = "#<buffer "//text//">"
            else
                s = "#<closed buffer>"
            end if
        case (V_MARKER)
            s = marker_text(xhandle(object))
        case (V_KEYMAP)
            if (is_keymap_live(xhandle(object)) .and. .not. is_keymap_full(xhandle(object))) then
                s = "("//keymap_elements(xhandle(object))//")"
            else
                s = "#<keymap>"
            end if
        case default
            s = "#<unknown>"
        end select
    end function readable_text

    !> OBJECT written as Fortran, as help shows a value: .false. for nil,
    !> .true. for t, integers, strings as Fortran literals (doubling each
    !> double quote), other symbols by name (as named constants are
    !> written), lists as array constructors; buffers, markers and keymaps,
    !> which have no Fortran notation, as `readable_text' gives them.
    !> (Ours: Fortmacs shows values as Fortran, not as Lisp.)  Not a port.
    recursive function fortran_text(object) result(s)
        type(value_type), intent(in) :: object
        character(len=:), allocatable :: s
        type(value_type), allocatable :: elements(:)
        type(status_type) :: status
        integer :: k

        select case (vtag(object))
        case (V_NULL)
            s = ".false."
        case (V_NAME)
            call symbol_name(object, s, status)

            if (.not. status%ok()) then
                s = "#<symbol>"
            else if (s == "t") then
                s = ".true."
            end if
        case (V_STRING)
            s = fortran_string(xstring(object))
        case (V_ARRAY)
            call list_elements(object, elements)
            s = "["

            do k = 1, size(elements)
                if (k > 1) s = s//", "
                s = s//fortran_text(elements(k))
            end do

            s = s//"]"
        case default
            s = readable_text(object)
        end select
    end function fortran_text

    !> The string TEXT as a Fortran literal: in double quotes, each double
    !> quote doubled.  (Ours: how help and error messages show a string.)
    !> Not a port.
    pure function fortran_string(text) result(s)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        integer :: k

        s = """"

        do k = 1, len(text)
            s = s//text(k:k)
            if (text(k:k) == """") s = s//""""
        end do

        s = s//""""
    end function fortran_string

    !> The printed representation of the string TEXT, as `prin1' writes
    !> it: in double quotes, a backslash escaping each double quote and
    !> backslash.  Port of the string case of print.c:print_object.
    pure function quoted_text(text) result(s)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        integer :: k

        s = """"

        do k = 1, len(text)
            if (text(k:k) == """" .or. text(k:k) == "\") s = s//"\"
            s = s//text(k:k)
        end do

        s = s//""""
    end function quoted_text

    !> "#<marker at N in BUFFER>", "#<marker in no buffer>", after
    !> "(moves after insertion) " for that insertion type.  Port of
    !> print.c:print_vectorlike (the marker case).
    function marker_text(m) result(s)
        type(handle_type), intent(in) :: m
        character(len=:), allocatable :: s
        character(len=:), allocatable :: name
        type(status_type) :: status
        character(len=21) :: buffer
        integer(int64) :: i

        s = "#<marker "
        call resolve_marker(m, i, status)

        if (.not. status%ok()) then
            s = s//"in no buffer>"
            return
        end if

        if (markers(i)%insertion_type) s = s//"(moves after insertion) "

        if (markers(i)%buffer == 0) then
            s = s//"in no buffer"
        else
            write (buffer, "(i0)") markers(i)%character_position
            call marker_buffer_name(markers(i)%buffer, name)
            s = s//"at "//trim(buffer)//" in "//name
        end if

        s = s//">"
    end function marker_text

    !> The name of the buffer in registry slot `slot'.  Not a port.
    subroutine marker_buffer_name(slot, name)
        integer(int64), intent(in) :: slot
        character(len=:), allocatable, intent(out) :: name

        name = buffers(slot)%name
    end subroutine marker_buffer_name

    !> The elements of keymap `km''s list printed, "keymap (97 . undo)
    !> ...": its bindings newest first, its prompt, then its parent's.
    !> As print.c prints the list (keymap ENTRIES... PROMPT . PARENT).
    !> Not a port.
    recursive function keymap_elements(km) result(s)
        type(handle_type), intent(in) :: km
        character(len=:), allocatable :: s
        type(value_type), allocatable :: events(:), bindings(:), range_bindings(:)
        integer(int64), allocatable :: range_from(:), range_to(:)
        type(value_type) :: prompt
        type(handle_type) :: parent
        character(len=21) :: a, b
        integer :: k

        s = "keymap"
        call keymap_entries(km, events, bindings, range_from, range_to, range_bindings)

        do k = 1, size(range_from)
            write (a, "(i0)") range_from(k)
            write (b, "(i0)") range_to(k)
            s = s//" "//keymap_entry_text("("//trim(a)//" . "//trim(b)//")", range_bindings(k))
        end do

        do k = 1, size(events)
            s = s//" "//keymap_entry_text(readable_text(events(k)), bindings(k))
        end do

        prompt = keymap_own_prompt(km)
        if (vtag(prompt) /= V_NULL) s = s//" "//readable_text(prompt)
        parent = keymap_parent(km)

        if (is_keymap_live(parent)) then
            if (is_keymap_full(parent)) then
                s = s//" . #<keymap>"
            else
                s = s//" "//keymap_elements(parent)
            end if
        end if
    end function keymap_elements

    !> A keymap entry printed, as Lisp prints a keymap: EVENT (already
    !> printed) and its BINDING: "(EVENT)" for no binding, "(EVENT
    !> ELEMENTS...)" for an array or a keymap, else "(EVENT . BINDING)" as
    !> upstream prints it.
    !> Not a port.
    recursive function keymap_entry_text(event, binding) result(s)
        character(len=*), intent(in) :: event
        type(value_type), intent(in) :: binding
        character(len=:), allocatable :: s
        character(len=:), allocatable :: inner

        select case (vtag(binding))
        case (V_NULL)
            s = "("//event//")"
        case (V_ARRAY)
            inner = readable_text(binding)
            s = "("//event//" "//inner(2:)
        case (V_KEYMAP)
            if (is_keymap_live(xhandle(binding)) .and. .not. is_keymap_full(xhandle(binding))) then
                s = "("//event//" "//keymap_elements(xhandle(binding))//")"
            else
                s = "("//event//" . #<keymap>)"
            end if
        case default
            s = "("//event//" . "//readable_text(binding)//")"
        end select
    end function keymap_entry_text

    ! ------------------------------------------------------------------
    ! Floats
    ! ------------------------------------------------------------------

    !> The printed representation of the real64 X: the fewest digits
    !> that read back as X, with a decimal point and a digit after it or
    !> an exponent ("1.0", "0.1", "1e+20"), "1.0e+INF" or "-1.0e+INF" for
    !> an infinity, "0.0e+NaN" (its payload before ".0", a minus sign for
    !> a negative one) for a NaN.  Port of print.c:float_to_string (with
    !> Gnulib lib/ftoastr.c:dtoastr; `float_output_format' unset).
    function float_to_string(x) result(s)
        real(real64), intent(in) :: x
        character(len=:), allocatable :: s
        character(len=24) :: buffer
        real(real64) :: y
        integer(int64) :: precision
        integer :: ios, k

        if (ieee_is_nan(x)) then
            write (buffer, "(i0)") iand(transfer(x, 0_int64), 2_int64**51 - 1)
            s = trim(buffer)//".0e+NaN"
            if (sign(1.0_real64, x) < 0) s = "-"//s
            return
        end if

        if (.not. ieee_is_finite(x)) then
            s = "1.0e+INF"
            if (x < 0) s = "-"//s
            return
        end if

        precision = 15
        if (abs(x) < tiny(x)) precision = 1

        do
            s = sprintf_real(x, "g", precision, .false., .false., .false.)
            read (s, *, iostat=ios) y
            if ((ios == 0 .and. y == x) .or. precision >= 17) exit
            precision = precision + 1
        end do

        ! The decimal point must be printed (Bug#8033).
        k = verify(s, "-0123456789")

        if (k == 0) then
            s = s//".0"
        else if (k == len(s)) then
            if (s(k:k) == ".") s = s//"0"
        end if
    end function float_to_string

    !> The real64 X as C's printf writes it with the conversion
    !> CONVERSION ("e", "f" or "g"), the precision PRECISION (negative:
    !> the default, 6) and the flags + (PLUS_FLAG), space (SPACE_FLAG)
    !> and # (SHARP_FLAG); "inf" and "nan" with their sign.  Digits are
    !> rounded to nearest, ties to even, from the exact value.
    !> Not a port.
    function sprintf_real(x, conversion, precision, plus_flag, space_flag, sharp_flag) result(s)
        real(real64), intent(in) :: x
        character, intent(in) :: conversion
        integer(int64), intent(in) :: precision
        logical, intent(in) :: plus_flag, space_flag, sharp_flag
        character(len=:), allocatable :: s
        character(len=:), allocatable :: digits
        integer(int64) :: point

        if (ieee_is_nan(x)) then
            s = sign_prefix(sign(1.0_real64, x) < 0, plus_flag, space_flag)//"nan"
        else if (.not. ieee_is_finite(x)) then
            s = sign_prefix(x < 0, plus_flag, space_flag)//"inf"
        else
            call exact_decimal(x, digits, point)
            s = sign_prefix(sign(1.0_real64, x) < 0, plus_flag, space_flag)// &
                sprintf_digits(digits, point, conversion, precision, sharp_flag)
        end if
    end function sprintf_real

    !> The integer N as `sprintf_real' writes a real64, from its exact
    !> value (upstream formats it as a long double, which holds every
    !> int64).  Not a port.
    function sprintf_integer(n, conversion, precision, plus_flag, space_flag, sharp_flag) result(s)
        integer(int64), intent(in) :: n
        character, intent(in) :: conversion
        integer(int64), intent(in) :: precision
        logical, intent(in) :: plus_flag, space_flag, sharp_flag
        character(len=:), allocatable :: s
        character(len=:), allocatable :: digits
        character(len=24) :: buffer
        integer(int64) :: point, last

        write (buffer, "(i0)") n
        digits = trim(adjustl(buffer))
        if (digits(1:1) == "-") digits = digits(2:)
        point = len(digits, kind=int64)
        last = verify(digits, "0", back=.true.)
        digits = digits(1:last)
        s = sign_prefix(n < 0, plus_flag, space_flag)//sprintf_digits(digits, point, conversion, precision, sharp_flag)
    end function sprintf_integer

    !> "-" if NEGATIVE, else "+" for PLUS_FLAG, " " for SPACE_FLAG, else
    !> nothing (printf's sign).  Not a port.
    pure function sign_prefix(negative, plus_flag, space_flag) result(s)
        logical, intent(in) :: negative, plus_flag, space_flag
        character(len=:), allocatable :: s

        if (negative) then
            s = "-"
        else if (plus_flag) then
            s = "+"
        else if (space_flag) then
            s = " "
        else
            s = ""
        end if
    end function sign_prefix

    !> The exact decimal value of the finite |X|: 0.DIGITS times ten to
    !> the power POINT, DIGITS without leading or trailing zeros (empty
    !> for zero).  A real64 has at most 767 significant digits, and the
    !> processor writes them exactly.  Not a port.
    subroutine exact_decimal(x, digits, point)
        real(real64), intent(in) :: x
        character(len=:), allocatable, intent(out) :: digits
        integer(int64), intent(out) :: point
        character(len=1100) :: buffer
        integer :: e, last, power

        if (x == 0) then
            digits = ""
            point = 0
            return
        end if

        write (buffer, "(ES1100.1080E5)") abs(x)
        buffer = adjustl(buffer)
        e = index(buffer, "E")
        read (buffer(e + 1:), *) power
        digits = buffer(1:1)//buffer(3:e - 1)
        last = verify(digits, "0", back=.true.)
        digits = digits(1:last)
        point = power + 1
    end subroutine exact_decimal

    !> DIGITS (as `exact_decimal' gives them) rounded to their first N
    !> digits, to nearest with ties to even: ROUNDED has N digits, zeros
    !> added, or "1" and N zeros when they rounded up to the next power
    !> of ten (CARRY true).  Not a port.
    pure subroutine round_digits(digits, n, rounded, carry)
        character(len=*), intent(in) :: digits
        integer(int64), intent(in) :: n
        character(len=:), allocatable, intent(out) :: rounded
        logical, intent(out) :: carry
        logical :: up
        integer(int64) :: k

        carry = .false.

        if (n >= len(digits, kind=int64)) then
            rounded = digits//repeat("0", int(n - len(digits)))
            return
        end if

        rounded = digits(1:n)

        select case (digits(n + 1:n + 1))
        case ("6":"9")
            up = .true.
        case ("5")
            ! DIGITS has no trailing zeros: more digits mean above the tie.
            up = len(digits, kind=int64) > n + 1

            if (.not. up .and. n > 0) then
                up = mod(iachar(digits(n:n)), 2) == 1
            end if
        case default
            up = .false.
        end select

        if (.not. up) return

        do k = n, 1, -1
            if (rounded(k:k) /= "9") then
                rounded(k:k) = achar(iachar(rounded(k:k)) + 1)
                return
            end if

            rounded(k:k) = "0"
        end do

        rounded = "1"//rounded
        carry = .true.
    end subroutine round_digits

    !> The magnitude 0.DIGITS times ten to the power POINT written by
    !> CONVERSION (e, f or g) with PRECISION (negative: 6) and the #
    !> flag SHARP_FLAG, as C's printf does.  Not a port.
    function sprintf_digits(digits, point, conversion, precision, sharp_flag) result(s)
        character(len=*), intent(in) :: digits
        integer(int64), intent(in) :: point
        character, intent(in) :: conversion
        integer(int64), intent(in) :: precision
        logical, intent(in) :: sharp_flag
        character(len=:), allocatable :: s
        character(len=:), allocatable :: rounded
        integer(int64) :: p, x, k, e
        logical :: carry

        p = precision
        if (p < 0) p = 6

        select case (conversion)
        case ("e")
            s = e_digits(digits, point, p, sharp_flag)
        case ("f")
            s = f_digits(digits, point, p, sharp_flag)
        case default
            ! %g: %e's exponent X for precision P decides; trailing zeros
            ! go unless the # flag.
            if (p == 0) p = 1
            x = 0

            if (len(digits) > 0) then
                call round_digits(digits, p, rounded, carry)
                x = point - 1
                if (carry) x = x + 1
            end if

            if (p > x .and. x >= -4) then
                s = f_digits(digits, point, p - 1 - x, sharp_flag)
            else
                s = e_digits(digits, point, p - 1, sharp_flag)
            end if

            if (.not. sharp_flag .and. index(s, ".") > 0) then
                e = index(s, "e")
                if (e == 0) e = len(s, kind=int64) + 1
                k = verify(s(1:e - 1), "0", back=.true.)
                if (s(k:k) == ".") k = k - 1
                s = s(1:k)//s(e:)
            end if
        end select
    end function sprintf_digits

    !> printf's %e of 0.DIGITS times ten to the power POINT with
    !> PRECISION digits after the point.  Not a port.
    function e_digits(digits, point, precision, sharp_flag) result(s)
        character(len=*), intent(in) :: digits
        integer(int64), intent(in) :: point, precision
        logical, intent(in) :: sharp_flag
        character(len=:), allocatable :: s
        character(len=:), allocatable :: rounded
        character(len=24) :: buffer
        integer(int64) :: x
        logical :: carry

        if (len(digits) == 0) then
            rounded = repeat("0", int(precision + 1))
            x = 0
        else
            call round_digits(digits, precision + 1, rounded, carry)
            x = point - 1
            if (carry) x = x + 1
        end if

        s = rounded(1:1)
        if (precision > 0 .or. sharp_flag) s = s//"."//rounded(2:precision + 1)

        if (abs(x) < 100) then
            write (buffer, "(i2.2)") abs(x)
        else
            write (buffer, "(i0)") abs(x)
        end if

        if (x < 0) then
            s = s//"e-"//trim(buffer)
        else
            s = s//"e+"//trim(buffer)
        end if
    end function e_digits

    !> printf's %f of 0.DIGITS times ten to the power POINT with
    !> PRECISION digits after the point.  Not a port.
    function f_digits(digits, point, precision, sharp_flag) result(s)
        character(len=*), intent(in) :: digits
        integer(int64), intent(in) :: point, precision
        logical, intent(in) :: sharp_flag
        character(len=:), allocatable :: s
        character(len=:), allocatable :: rounded, whole, fraction
        integer(int64) :: keep, p
        logical :: carry

        keep = point + precision

        if (len(digits) == 0 .or. keep < 0) then
            whole = "0"
            fraction = repeat("0", int(precision))
        else
            call round_digits(digits, keep, rounded, carry)
            p = point
            if (carry) p = p + 1

            if (p <= 0) then
                whole = "0"
                fraction = repeat("0", int(-p))//rounded
            else
                whole = rounded(1:p)
                fraction = rounded(p + 1:)
            end if
        end if

        s = whole
        if (precision > 0 .or. sharp_flag) s = s//"."//fraction
    end function f_digits
end module print_mod
