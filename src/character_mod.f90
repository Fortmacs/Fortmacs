! character_mod.f90 -- multibyte character encoding.
! Ported from GNU Emacs 31.1 src/character.c and src/character.h.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1997, 1998, 2001 Electrotechnical Laboratory, JAPAN.
!   Licensed to the Free Software Foundation.
! Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

!> Emacs's internal multibyte encoding: UTF-8 extended to 22-bit codes,
!> plus two-byte forms for raw 8-bit bytes.  From character.h:
!>
!>     character code   1st byte   byte sequence
!>     --------------   --------   -------------
!>          0-7F        00..7F     0xxxxxxx
!>         80-7FF       C2..DF     110yyyyx 10xxxxxx
!>        800-FFFF      E0..EF     1110yyyy 10yxxxxx 10xxxxxx
!>      10000-1FFFFF    F0..F7     11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
!>     200000-3FFF7F    F8         11111000 1000yxxx 10xxxxxx 10xxxxxx 10xxxxxx
!>     3FFF80-3FFFFF    C0..C1     1100000x 10xxxxxx  (eight-bit-char)
!>     400000-...                  invalid
!>
!> Valid Unicode text is therefore plain UTF-8.  Byte strings are Fortran
!> default-kind `character' (each element one byte) and byte positions are
!> 1-based `int64'.  Character codes are `int32' (they include modifier
!> bits up to CHARACTER_META).
!>
!> Where upstream takes a `check' flag and an end pointer, these procedures
!> always bound-check against `len(string)'; the unchecked fast paths are an
!> optimization C needs and Fortran (with -fcheck=all) does not.
module character_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use unicode_category_mod, only: category_runs, category_starts, category_ends, category_codes
    implicit none(type, external)
    private

    ! character.h
    !> The largest character code.  Port of character.h MAXIMUM_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_CHARACTER = int(z'3FFFFF', int32)
    !> The largest Unicode character.  Port of character.h MAXIMUM_UNICODE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_UNICODE_CHARACTER = int(z'10FFFF', int32)
    !> The largest character whose multibyte form has 1 byte.  Port of
    !> character.h MAXIMUM_1_BYTE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_1_BYTE_CHARACTER = int(z'7F', int32)
    !> The largest character whose multibyte form has 2 bytes.  Port of
    !> character.h MAXIMUM_2_BYTE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_2_BYTE_CHARACTER = int(z'7FF', int32)
    !> The largest character whose multibyte form has 3 bytes.  Port of
    !> character.h MAXIMUM_3_BYTE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_3_BYTE_CHARACTER = int(z'FFFF', int32)
    !> The largest character whose multibyte form has 4 bytes.  Port of
    !> character.h MAXIMUM_4_BYTE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_4_BYTE_CHARACTER = int(z'1FFFFF', int32)
    !> The largest character whose multibyte form has 5 bytes.  Port of
    !> character.h MAXIMUM_5_BYTE_CHARACTER.
    integer(int32), parameter, public :: MAXIMUM_5_BYTE_CHARACTER = int(z'3FFF7F', int32)
    !> The smallest leading byte of a multibyte form.  Port of character.h
    !> MINIMUM_MULTIBYTE_LEADING_CODE.
    integer(int32), parameter, public :: MINIMUM_MULTIBYTE_LEADING_CODE = int(z'C0', int32)
    !> The largest leading byte of a multibyte form.  Port of character.h
    !> MAXIMUM_MULTIBYTE_LEADING_CODE.
    integer(int32), parameter, public :: MAXIMUM_MULTIBYTE_LEADING_CODE = int(z'F8', int32)
    !> The longest multibyte form, in bytes.  Port of character.h
    !> MAXIMUM_MULTIBYTE_LENGTH.
    integer, parameter, public :: MAXIMUM_MULTIBYTE_LENGTH = 5

    !> lisp.h: modifier bits of character events.
    !> The alt modifier bit of a character event.  Port of lisp.h CHARACTER_ALT.
    integer(int32), parameter, public :: CHARACTER_ALT = int(z'0400000', int32)
    !> The super modifier bit of a character event.  Port of lisp.h CHARACTER_SUPER.
    integer(int32), parameter, public :: CHARACTER_SUPER = int(z'0800000', int32)
    !> The hyper modifier bit of a character event.  Port of lisp.h CHARACTER_HYPER.
    integer(int32), parameter, public :: CHARACTER_HYPER = int(z'1000000', int32)
    !> The shift modifier bit of a character event.  Port of lisp.h CHARACTER_SHIFT.
    integer(int32), parameter, public :: CHARACTER_SHIFT = int(z'2000000', int32)
    !> The control modifier bit of a character event.  Port of lisp.h CHARACTER_CONTROL.
    integer(int32), parameter, public :: CHARACTER_CONTROL = int(z'4000000', int32)
    !> The meta modifier bit of a character event.  Port of lisp.h CHARACTER_META.
    integer(int32), parameter, public :: CHARACTER_META = int(z'8000000', int32)
    !> All the modifier bits.  Port of lisp.h CHARACTER_MODIFIER_MASK.
    integer(int32), parameter, public :: CHARACTER_MODIFIER_MASK = &
                                         ior(ior(ior(CHARACTER_ALT, CHARACTER_SUPER), ior(CHARACTER_HYPER, CHARACTER_SHIFT)), &
                                             ior(CHARACTER_CONTROL, CHARACTER_META))

    integer(int32), parameter :: byte8_offset = int(z'3FFF00', int32)

    public :: is_ascii_character, is_single_byte_character, is_character_valid
    public :: is_character_byte8, byte8_to_character, unibyte_to_character, character_to_byte8, &
              character_to_byte_safe, is_character_byte8_head
    public :: character_bytes, character_leading_code, character_string
    public :: is_leading_code, is_trailing_code, is_character_head, bytes_by_character_head
    public :: multibyte_length, raw_previous_character_length, string_character_and_length, &
              string_character
    public :: multibyte_characters_in_text, parse_string_as_multibyte, &
              string_as_multibyte, count_size_as_multibyte, string_to_multibyte, &
              string_as_unibyte
    public :: character_resolve_modifier_mask
    public :: variation_selector_number, is_character_surrogate, character_hexdigit
    public :: downcase_ascii
    public :: unicode_category, is_alphabetic, is_alphanumeric, is_graphic, is_printable, is_blank, glyph_width

    !> Unicode general categories (character.h unicode_category_t).
    integer, parameter, public :: UNICODE_CATEGORY_UNKNOWN = 0, UNICODE_CATEGORY_LU = 1, &
                                  UNICODE_CATEGORY_LL = 2, UNICODE_CATEGORY_LT = 3, UNICODE_CATEGORY_LM = 4, &
                                  UNICODE_CATEGORY_LO = 5, UNICODE_CATEGORY_MN = 6, UNICODE_CATEGORY_MC = 7, &
                                  UNICODE_CATEGORY_ME = 8, UNICODE_CATEGORY_ND = 9, UNICODE_CATEGORY_NEWLINE = 10, &
                                  UNICODE_CATEGORY_NO = 11, UNICODE_CATEGORY_PC = 12, UNICODE_CATEGORY_PD = 13, &
                                  UNICODE_CATEGORY_PS = 14, UNICODE_CATEGORY_PE = 15, UNICODE_CATEGORY_PI = 16, &
                                  UNICODE_CATEGORY_PF = 17, UNICODE_CATEGORY_PO = 18, UNICODE_CATEGORY_SM = 19, &
                                  UNICODE_CATEGORY_SC = 20, UNICODE_CATEGORY_SK = 21, UNICODE_CATEGORY_SO = 22, &
                                  UNICODE_CATEGORY_ZS = 23, UNICODE_CATEGORY_ZL = 24, UNICODE_CATEGORY_ZP = 25, &
                                  UNICODE_CATEGORY_CC = 26, UNICODE_CATEGORY_CF = 27, UNICODE_CATEGORY_CS = 28, &
                                  UNICODE_CATEGORY_CO = 29, UNICODE_CATEGORY_CN = 30

contains

    !> S with its ASCII letters A-Z in lowercase (other bytes unchanged).
    !> Not a port.
    pure function downcase_ascii(s) result(lower)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: lower
        integer :: i

        lower = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") lower(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase_ascii

    !> Port of lisp.h:ASCII_CHAR_P.
    elemental logical function is_ascii_character(c)
        integer(int32), intent(in) :: c

        is_ascii_character = 0 <= c .and. c < 128
    end function is_ascii_character

    !> Port of character.h:SINGLE_BYTE_CHAR_P.
    elemental logical function is_single_byte_character(c)
        integer(int32), intent(in) :: c

        is_single_byte_character = 0 <= c .and. c < 256
    end function is_single_byte_character

    !> Port of character.h:CHAR_VALID_P.
    elemental logical function is_character_valid(c)
        integer(int32), intent(in) :: c

        is_character_valid = 0 <= c .and. c <= MAXIMUM_CHARACTER
    end function is_character_valid

    !> True if `c' stands for a raw 8-bit byte.
    !> Port of character.h:CHAR_BYTE8_P.
    elemental logical function is_character_byte8(c)
        integer(int32), intent(in) :: c

        is_character_byte8 = MAXIMUM_5_BYTE_CHARACTER < c
    end function is_character_byte8

    !> Character code for raw byte `byte'.
    !> Port of character.h:BYTE8_TO_CHAR.
    elemental integer(int32) function byte8_to_character(byte)
        integer(int32), intent(in) :: byte

        byte8_to_character = byte + byte8_offset
    end function byte8_to_character

    !> Port of character.h:UNIBYTE_TO_CHAR.
    elemental integer(int32) function unibyte_to_character(byte)
        integer(int32), intent(in) :: byte

        if (is_ascii_character(byte)) then
            unibyte_to_character = byte
        else
            unibyte_to_character = byte8_to_character(byte)
        end if
    end function unibyte_to_character

    !> Raw byte for character `c'.  Port of character.h:CHAR_TO_BYTE8.
    elemental integer(int32) function character_to_byte8(c)
        integer(int32), intent(in) :: c

        if (is_character_byte8(c)) then
            character_to_byte8 = c - byte8_offset
        else
            character_to_byte8 = iand(c, 255_int32)
        end if
    end function character_to_byte8

    !> Raw byte for `c', or -1 if it is not one.
    !> Port of character.h:CHAR_TO_BYTE_SAFE.
    elemental integer(int32) function character_to_byte_safe(c)
        integer(int32), intent(in) :: c

        if (is_ascii_character(c)) then
            character_to_byte_safe = c
        else if (is_character_byte8(c)) then
            character_to_byte_safe = c - byte8_offset
        else
            character_to_byte_safe = -1
        end if
    end function character_to_byte_safe

    !> True if `byte' leads the two-byte form of a raw byte.
    !> Port of character.h:CHAR_BYTE8_HEAD_P.
    elemental logical function is_character_byte8_head(byte)
        integer(int32), intent(in) :: byte

        is_character_byte8_head = byte == int(z'C0', int32) .or. byte == int(z'C1', int32)
    end function is_character_byte8_head

    !> Length of the multibyte form of `c'.  Port of character.h:CHAR_BYTES.
    elemental integer(int32) function character_bytes(c)
        integer(int32), intent(in) :: c

        if (c <= MAXIMUM_1_BYTE_CHARACTER) then
            character_bytes = 1
        else if (c <= MAXIMUM_2_BYTE_CHARACTER) then
            character_bytes = 2
        else if (c <= MAXIMUM_3_BYTE_CHARACTER) then
            character_bytes = 3
        else if (c <= MAXIMUM_4_BYTE_CHARACTER) then
            character_bytes = 4
        else if (c <= MAXIMUM_5_BYTE_CHARACTER) then
            character_bytes = 5
        else
            character_bytes = 2
        end if
    end function character_bytes

    !> First byte of the multibyte form of `c'.
    !> Port of character.h:CHAR_LEADING_CODE.
    elemental integer(int32) function character_leading_code(c)
        integer(int32), intent(in) :: c

        if (c <= MAXIMUM_1_BYTE_CHARACTER) then
            character_leading_code = c
        else if (c <= MAXIMUM_2_BYTE_CHARACTER) then
            character_leading_code = ior(int(z'C0', int32), ishft(c, -6))
        else if (c <= MAXIMUM_3_BYTE_CHARACTER) then
            character_leading_code = ior(int(z'E0', int32), ishft(c, -12))
        else if (c <= MAXIMUM_4_BYTE_CHARACTER) then
            character_leading_code = ior(int(z'F0', int32), ishft(c, -18))
        else if (c <= MAXIMUM_5_BYTE_CHARACTER) then
            character_leading_code = int(z'F8', int32)
        else
            character_leading_code = ior(int(z'C0', int32), iand(ishft(c, -6), 1_int32))
        end if
    end function character_leading_code

    !> Store the multibyte form of `c' in `buffer(1:nbytes)'.  Modifier bits
    !> are folded in where possible and otherwise dropped.  Fails for codes
    !> outside 0..MAXIMUM_CHARACTER.
    !> Port of character.c:char_string and character.h:CHAR_STRING,
    !> BYTE8_STRING.
    pure subroutine character_string(c, buffer, nbytes, status)
        integer(int32), intent(in) :: c
        character(len=MAXIMUM_MULTIBYTE_LENGTH), intent(out) :: buffer
        integer(int32), intent(out) :: nbytes
        type(status_type), intent(out) :: status
        integer(int32) :: k
        character(len=8) :: hexadecimal
        integer(int64) :: uk

        buffer = ""
        nbytes = 0
        k = c

        if (iand(k, CHARACTER_MODIFIER_MASK) /= 0) then
            k = character_resolve_modifier_mask(k)
            ! If C still has any modifier bits, just ignore it.
            k = iand(k, not(CHARACTER_MODIFIER_MASK))
        end if

        ! C is unsigned upstream: a negative code is a huge one.
        if (k < 0 .or. k > MAXIMUM_CHARACTER) then
            ! error ("Invalid character: %x", c), with the resolved C.
            uk = int(k, int64)
            if (uk < 0) uk = uk + 2_int64**32
            write (hexadecimal, "(z0)") uk
            call set_error(status, GENERIC_ERROR_CODE, "Invalid character: "//lower_hexadecimal(trim(hexadecimal)))
            return
        end if

        if (k <= MAXIMUM_1_BYTE_CHARACTER) then
            buffer(1:1) = char(k)
            nbytes = 1
        else if (k <= MAXIMUM_2_BYTE_CHARACTER) then
            buffer(1:1) = char(ior(int(z'C0', int32), ishft(k, -6)))
            buffer(2:2) = char(trail(k))
            nbytes = 2
        else if (k <= MAXIMUM_3_BYTE_CHARACTER) then
            buffer(1:1) = char(ior(int(z'E0', int32), ishft(k, -12)))
            buffer(2:2) = char(trail(ishft(k, -6)))
            buffer(3:3) = char(trail(k))
            nbytes = 3
        else if (k <= MAXIMUM_4_BYTE_CHARACTER) then
            buffer(1:1) = char(ior(int(z'F0', int32), ishft(k, -18)))
            buffer(2:2) = char(trail(ishft(k, -12)))
            buffer(3:3) = char(trail(ishft(k, -6)))
            buffer(4:4) = char(trail(k))
            nbytes = 4
        else if (k <= MAXIMUM_5_BYTE_CHARACTER) then
            buffer(1:1) = char(int(z'F8', int32))
            buffer(2:2) = char(ior(int(z'80', int32), iand(ishft(k, -18), int(z'0F', int32))))
            buffer(3:3) = char(trail(ishft(k, -12)))
            buffer(4:4) = char(trail(ishft(k, -6)))
            buffer(5:5) = char(trail(k))
            nbytes = 5
        else
            ! BYTE8_STRING
            k = character_to_byte8(k)
            buffer(1:1) = char(ior(int(z'C0', int32), iand(ishft(k, -6), 1_int32)))
            buffer(2:2) = char(trail(k))
            nbytes = 2
        end if

    contains

        !> 10xxxxxx from the low six bits of `x'.
        pure integer(int32) function trail(x)
            integer(int32), intent(in) :: x

            trail = ior(int(z'80', int32), iand(x, int(z'3F', int32)))
        end function trail

        !> HEXADECIMAL with its letters in lower case, as printf's %x writes them.
        pure function lower_hexadecimal(hexadecimal) result(s)
            character(len=*), intent(in) :: hexadecimal
            character(len=len(hexadecimal)) :: s
            integer :: i

            s = hexadecimal

            do i = 1, len(s)
                if (s(i:i) >= "A" .and. s(i:i) <= "F") s(i:i) = achar(iachar(s(i:i)) + 32)
            end do
        end function lower_hexadecimal
    end subroutine character_string

    !> Port of character.h:LEADING_CODE_P.
    elemental logical function is_leading_code(byte)
        integer(int32), intent(in) :: byte

        is_leading_code = iand(byte, int(z'C0', int32)) == int(z'C0', int32)
    end function is_leading_code

    !> Port of character.h:TRAILING_CODE_P.
    elemental logical function is_trailing_code(byte)
        integer(int32), intent(in) :: byte

        is_trailing_code = iand(byte, int(z'C0', int32)) == int(z'80', int32)
    end function is_trailing_code

    !> True if `byte' starts a character.  Port of character.h:CHAR_HEAD_P.
    elemental logical function is_character_head(byte)
        integer(int32), intent(in) :: byte

        is_character_head = iand(byte, int(z'C0', int32)) /= int(z'80', int32)
    end function is_character_head

    !> Length of a character judged by its first byte alone (no validation).
    !> Port of character.h:BYTES_BY_CHAR_HEAD.
    elemental integer(int32) function bytes_by_character_head(byte)
        integer(int32), intent(in) :: byte

        if (iand(byte, int(z'80', int32)) == 0) then
            bytes_by_character_head = 1
        else if (iand(byte, int(z'20', int32)) == 0) then
            bytes_by_character_head = 2
        else if (iand(byte, int(z'10', int32)) == 0) then
            bytes_by_character_head = 3
        else if (iand(byte, int(z'08', int32)) == 0) then
            bytes_by_character_head = 4
        else
            bytes_by_character_head = 5
        end if
    end function bytes_by_character_head

    !> Byte length of the valid multibyte form starting at `string(pos:)', or 0
    !> if there is none.  With `allow_8bit', raw-byte forms (C0/C1) count.
    !> Port of character.h:multibyte_length (always with check = true).
    pure integer(int32) function multibyte_length(string, position, allow_8bit)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: position
        logical, intent(in) :: allow_8bit
        integer(int64) :: pend
        integer(int32) :: c, d, e, f, w, w1, w2, lo2
        integer(int64) :: lw, w3

        multibyte_length = 0
        pend = len(string, kind=int64) + 1
        if (position >= pend) return
        c = ichar(string(position:position), kind=int32)

        if (c < int(z'80', int32)) then
            multibyte_length = 1
            return
        end if

        if (position + 1 >= pend) return
        d = ichar(string(position + 1:position + 1), kind=int32)
        w = ishft(iand(d, int(z'C0', int32)), 2) + c
        lo2 = int(z'2C2', int32)
        if (allow_8bit) lo2 = int(z'2C0', int32)

        if (lo2 <= w .and. w <= int(z'2DF', int32)) then
            multibyte_length = 2
            return
        end if

        if (position + 2 >= pend) return
        e = ichar(string(position + 2:position + 2), kind=int32)
        w = w + ishft(iand(e, int(z'C0', int32)), 4)
        w1 = ior(w, ishft(iand(d, int(z'20', int32)), -2))

        if (int(z'AE1', int32) <= w1 .and. w1 <= int(z'AEF', int32)) then
            multibyte_length = 3
            return
        end if

        if (position + 3 >= pend) return
        f = ichar(string(position + 3:position + 3), kind=int32)
        w = w + ishft(iand(f, int(z'C0', int32)), 6)
        w2 = ior(w, ishft(iand(d, int(z'30', int32)), -3))

        if (int(z'2AF1', int32) <= w2 .and. w2 <= int(z'2AF7', int32)) then
            multibyte_length = 4
            return
        end if

        if (position + 4 >= pend) return
        lw = int(w, int64) + ishft(int(iand(ichar(string(position + 4:position + 4), kind=int32), int(z'C0', int32)), int64), 8)
        w3 = ishft(lw, 24) + ishft(int(d, int64), 16) + ishft(int(e, int64), 8) + int(f, int64)

        if (int(z'AAF8888080', int64) <= w3 .and. w3 <= int(z'AAF88FBFBD', int64)) then
            multibyte_length = 5
        end if
    end function multibyte_length

    !> Byte length of the character ending just before byte `position'.  Assumes
    !> `position' is a character boundary after the start of a valid text.
    !> Port of character.h:raw_prev_char_len.
    pure integer(int32) function raw_previous_character_length(string, position)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: position
        integer(int32) :: n

        ! Upstream loops without bound; stopping at the text start and at
        ! MAXIMUM_MULTIBYTE_LENGTH keeps malformed input from reading out of range.
        raw_previous_character_length = 1

        do n = 1, MAXIMUM_MULTIBYTE_LENGTH
            if (position - n < 1) exit
            raw_previous_character_length = n
            if (is_character_head(ichar(string(position - n:position - n), kind=int32))) return
        end do
    end function raw_previous_character_length

    !> Decode the character whose valid multibyte form starts at `string(pos:)'
    !> and return its byte length in `length'.
    !> Port of character.h:string_char_and_length.
    pure subroutine string_character_and_length(string, position, ch, length)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: position
        integer(int32), intent(out) :: ch, length
        integer(int32) :: c, d

        c = ichar(string(position:position), kind=int32)

        if (iand(c, int(z'80', int32)) == 0) then
            length = 1
            ch = c
            return
        end if

        d = ishft(c, 6) + ichar(string(position + 1:position + 1), kind=int32) - (ishft(int(z'C0', int32), 6) + int(z'80', int32))

        if (iand(c, int(z'20', int32)) == 0) then
            length = 2
            ch = d
            if (c < int(z'C2', int32)) ch = d + int(z'3FFF80', int32)
            return
        end if

        d = ishft(d, 6) + ichar(string(position + 2:position + 2), kind=int32) - (ishft(int(z'20', int32), 12) + int(z'80', int32))

        if (iand(c, int(z'10', int32)) == 0) then
            length = 3
            ch = d
            return
        end if

        d = ishft(d, 6) + ichar(string(position + 3:position + 3), kind=int32) - (ishft(int(z'10', int32), 18) + int(z'80', int32))

        if (iand(c, int(z'08', int32)) == 0) then
            length = 4
            ch = d
            return
        end if

        d = ishft(d, 6) + ichar(string(position + 4:position + 4), kind=int32) - (ishft(int(z'08', int32), 24) + int(z'80', int32))
        length = 5
        ch = d
    end subroutine string_character_and_length

    !> Decode the character at `string(pos:)'.  Port of character.h:STRING_CHAR.
    pure integer(int32) function string_character(string, position)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: position
        integer(int32) :: length

        call string_character_and_length(string, position, string_character, length)
    end function string_character

    !> Number of characters in multibyte text `string', which must contain only
    !> valid multibyte forms (raw-byte forms included).  Upstream aborts on
    !> an invalid sequence; here that is an error.
    !> Port of character.c:multibyte_chars_in_text.
    pure subroutine multibyte_characters_in_text(string, nchars, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: nchars
        type(status_type), intent(out) :: status
        integer(int64) :: p
        integer(int32) :: n

        nchars = 0
        p = 1

        do while (p <= len(string, kind=int64))
            n = multibyte_length(string, p, .true.)

            if (n == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid multibyte sequence")
                return
            end if

            p = p + n
            nchars = nchars + 1
        end do
    end subroutine multibyte_characters_in_text

    !> Count the characters and bytes unibyte text `string' would have as
    !> multibyte text: bytes that start no valid sequence become 2-byte
    !> raw-byte forms.  Port of character.c:parse_str_as_multibyte.
    pure subroutine parse_string_as_multibyte(string, nchars, nbytes)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: nchars, nbytes
        integer(int64) :: p
        integer(int32) :: n

        nchars = 0
        nbytes = 0
        p = 1

        do while (p <= len(string, kind=int64))
            n = multibyte_length(string, p, .false.)

            if (n > 0) then
                p = p + n
                nbytes = nbytes + n
            else
                p = p + 1
                nbytes = nbytes + 2
            end if

            nchars = nchars + 1
        end do
    end subroutine parse_string_as_multibyte

    !> Unibyte text `string' rearranged as multibyte text: valid sequences are
    !> kept, other 8-bit bytes become raw-byte forms.  `nchars' receives the
    !> character count.  Port of character.c:str_as_multibyte (which works
    !> in place; this produces a new string).
    pure subroutine string_as_multibyte(string, out, nchars)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: out
        integer(int64), intent(out), optional :: nchars
        integer(int64) :: p, q, chars, nbytes
        integer(int32) :: n, b

        call parse_string_as_multibyte(string, chars, nbytes)
        if (present(nchars)) nchars = chars
        allocate (character(len=nbytes) :: out)
        p = 1
        q = 1

        do while (p <= len(string, kind=int64))
            n = multibyte_length(string, p, .false.)

            if (n > 0) then
                out(q:q + n - 1) = string(p:p + n - 1)
                p = p + n
                q = q + n
            else
                b = ichar(string(p:p), kind=int32)
                out(q:q) = char(ior(int(z'C0', int32), iand(ishft(b, -6), 1_int32)))
                out(q + 1:q + 1) = char(ior(int(z'80', int32), iand(b, int(z'3F', int32))))
                p = p + 1
                q = q + 2
            end if
        end do
    end subroutine string_as_multibyte

    !> Bytes `string' occupies once every non-ASCII byte becomes a raw-byte
    !> form.  Port of character.c:count_size_as_multibyte.
    pure integer(int64) function count_size_as_multibyte(string)
        character(len=*), intent(in) :: string
        integer(int64) :: i

        count_size_as_multibyte = len(string, kind=int64)

        do i = 1, len(string, kind=int64)
            if (ichar(string(i:i), kind=int32) > 127) count_size_as_multibyte = count_size_as_multibyte + 1
        end do
    end function count_size_as_multibyte

    !> Unibyte `source' as multibyte text holding the same single-byte
    !> characters (every non-ASCII byte becomes a raw-byte form).
    !> Port of character.c:str_to_multibyte.
    pure function string_to_multibyte(source) result(destination)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: destination
        integer(int64) :: i, q
        integer(int32) :: b

        allocate (character(len=count_size_as_multibyte(source)) :: destination)
        q = 1

        do i = 1, len(source, kind=int64)
            b = ichar(source(i:i), kind=int32)

            if (b <= 127) then
                destination(q:q) = source(i:i)
                q = q + 1
            else
                destination(q:q) = char(int(z'C0', int32) + iand(ishft(b, -6), 1_int32))
                destination(q + 1:q + 1) = char(int(z'80', int32) + iand(b, int(z'3F', int32)))
                q = q + 2
            end if
        end do
    end function string_to_multibyte

    !> Multibyte `string' with raw-byte forms turned back into single bytes.
    !> Port of character.c:str_as_unibyte (which works in place).
    pure function string_as_unibyte(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out
        character(len=len(string)) :: work
        integer(int64) :: p, q
        integer(int32) :: c, n

        p = 1
        q = 0

        do while (p <= len(string, kind=int64))
            c = ichar(string(p:p), kind=int32)

            if (is_character_byte8_head(c)) then
                call string_character_and_length(string, p, c, n)
                q = q + 1
                work(q:q) = char(character_to_byte8(c))
            else
                n = min(bytes_by_character_head(c), int(len(string, kind=int64) - p + 1, int32))
                work(q + 1:q + n) = string(p:p + n - 1)
                q = q + n
            end if

            p = p + n
        end do

        out = work(1:q)
    end function string_as_unibyte

    !> Fold modifier bits of `c' into the code where ASCII allows it (e.g.
    !> C-a -> 1, S-a -> A).  Port of character.c:char_resolve_modifier_mask.
    elemental integer(int32) function character_resolve_modifier_mask(c) result(k)
        integer(int32), intent(in) :: c
        integer(int32) :: low

        k = c
        ! A non-ASCII character can't reflect modifier bits to the code.
        if (.not. is_ascii_character(iand(k, not(CHARACTER_MODIFIER_MASK)))) return

        if (iand(k, CHARACTER_SHIFT) /= 0) then
            low = iand(k, 255_int32)

            if (low >= iachar("A") .and. low <= iachar("Z")) then
                k = iand(k, not(CHARACTER_SHIFT))
            else if (low >= iachar("a") .and. low <= iachar("z")) then
                k = iand(k, not(CHARACTER_SHIFT)) - (iachar("a") - iachar("A"))
            else if (iand(k, not(CHARACTER_MODIFIER_MASK)) <= 32) then
                ! Shift is ignored for control characters and SPC.
                k = iand(k, not(CHARACTER_SHIFT))
            end if
        end if

        if (iand(k, CHARACTER_CONTROL) /= 0) then
            low = iand(k, 255_int32)

            if (low == iachar(" ")) then
                ! C-SPC -> NUL
                k = iand(k, not(ior(127_int32, CHARACTER_CONTROL)))
            else if (low == iachar("?")) then
                ! C-? -> DEL
                k = ior(127_int32, iand(k, not(ior(127_int32, CHARACTER_CONTROL))))
            else if (iand(k, 95_int32) >= 65 .and. iand(k, 95_int32) <= 90) then
                ! Letters, either case.
                k = iand(k, ior(31_int32, iand(not(127_int32), not(CHARACTER_CONTROL))))
            else if (iand(k, 127_int32) >= 64 .and. iand(k, 127_int32) <= 95) then
                ! The non-letters in 0100..0137 (@ [ \ ] ^ _).
                k = iand(k, ior(31_int32, iand(not(127_int32), not(CHARACTER_CONTROL))))
            end if
        end if
    end function character_resolve_modifier_mask

    !> Index (1..256) of variation selector `c', or 0.
    !> Port of character.h:CHAR_VARIATION_SELECTOR_P.
    elemental integer(int32) function variation_selector_number(c)
        integer(int32), intent(in) :: c

        if (c < int(z'FE00', int32)) then
            variation_selector_number = 0
        else if (c <= int(z'FE0F', int32)) then
            variation_selector_number = c - int(z'FE00', int32) + 1
        else if (c < int(z'E0100', int32)) then
            variation_selector_number = 0
        else if (c <= int(z'E01EF', int32)) then
            variation_selector_number = c - int(z'E0100', int32) + 17
        else
            variation_selector_number = 0
        end if
    end function variation_selector_number

    !> Port of character.h:char_surrogate_p.
    elemental logical function is_character_surrogate(c)
        integer(int32), intent(in) :: c

        is_character_surrogate = int(z'D800', int32) <= c .and. c <= int(z'DFFF', int32)
    end function is_character_surrogate

    !> Value 0..15 of hex digit `c', or -1.
    !> Port of character.h:char_hexdigit.
    elemental integer(int32) function character_hexdigit(c)
        integer(int32), intent(in) :: c

        if (c >= iachar("0") .and. c <= iachar("9")) then
            character_hexdigit = c - iachar("0")
        else if (c >= iachar("a") .and. c <= iachar("f")) then
            character_hexdigit = c - iachar("a") + 10
        else if (c >= iachar("A") .and. c <= iachar("F")) then
            character_hexdigit = c - iachar("A") + 10
        else
            character_hexdigit = -1
        end if
    end function character_hexdigit

    !> The Unicode general category of `c' (`UNICODE_CATEGORY_...'): Cn
    !> for an unassigned code point, UNKNOWN beyond Unicode.  What
    !> CHARACTER_TABLE_REFERENCE (Vunicode_category_table, c) gives upstream.
    !> Not a port.
    pure integer function unicode_category(c) result(category)
        integer(int32), intent(in) :: c
        integer :: lo, hi, middle

        category = UNICODE_CATEGORY_UNKNOWN
        if (c < 0 .or. c > MAXIMUM_UNICODE_CHARACTER) return
        category = UNICODE_CATEGORY_CN
        lo = 1
        hi = category_runs

        do while (lo <= hi)
            middle = (lo + hi)/2

            if (c < category_starts(middle)) then
                hi = middle - 1
            else if (c > category_ends(middle)) then
                lo = middle + 1
            else
                category = category_codes(middle)
                return
            end if
        end do
    end function unicode_category

    !> Return true if C is an alphabetic character.  Port of
    !> character.c:alphabeticp.
    pure logical function is_alphabetic(c)
        integer(int32), intent(in) :: c

        select case (unicode_category(c))
        case (UNICODE_CATEGORY_LU:UNICODE_CATEGORY_ME, UNICODE_CATEGORY_NEWLINE)
            is_alphabetic = .true.
        case default
            is_alphabetic = .false.
        end select
    end function is_alphabetic

    !> Return true if C is an alphabetic or decimal-number character.
    !> Port of character.c:alphanumericp.
    pure logical function is_alphanumeric(c)
        integer(int32), intent(in) :: c

        select case (unicode_category(c))
        case (UNICODE_CATEGORY_LU:UNICODE_CATEGORY_NEWLINE)
            is_alphanumeric = .true.
        case default
            is_alphanumeric = .false.
        end select
    end function is_alphanumeric

    !> Return true if C is a graphic character.  Port of
    !> character.c:graphicp.
    pure logical function is_graphic(c)
        integer(int32), intent(in) :: c

        select case (unicode_category(c))
        case (UNICODE_CATEGORY_UNKNOWN, UNICODE_CATEGORY_ZS:UNICODE_CATEGORY_CC, UNICODE_CATEGORY_CS, &
              UNICODE_CATEGORY_CN)
            is_graphic = .false.
        case default
            is_graphic = .true.
        end select
    end function is_graphic

    !> Return true if C is a printable character.  Port of
    !> character.c:printablep.
    pure logical function is_printable(c)
        integer(int32), intent(in) :: c

        select case (unicode_category(c))
        case (UNICODE_CATEGORY_UNKNOWN, UNICODE_CATEGORY_CC, UNICODE_CATEGORY_CS, UNICODE_CATEGORY_CN)
            is_printable = .false.
        case default
            is_printable = .true.
        end select
    end function is_printable

    !> Return true if C is a horizontal whitespace character, as defined
    !> by https://www.unicode.org/reports/tr18/tr18-19.html#blank.
    !> Port of character.c:blankp.
    pure logical function is_blank(c)
        integer(int32), intent(in) :: c

        is_blank = unicode_category(c) == UNICODE_CATEGORY_ZS
    end function is_blank

    !> Columns character `c' takes: 2 for East Asian wide and fullwidth
    !> characters and emoji, else 1.  An approximation of
    !> character.c:char_width with characters.el's char-width-table.
    !> Not a port.
    elemental integer function glyph_width(c)
        integer(int32), intent(in) :: c

        glyph_width = 1
        if ((c >= int(z'1100', int32) .and. c <= int(z'115F', int32)) .or. &
            (c >= int(z'2E80', int32) .and. c <= int(z'303E', int32)) .or. &
            (c >= int(z'3041', int32) .and. c <= int(z'33FF', int32)) .or. &
            (c >= int(z'3400', int32) .and. c <= int(z'4DBF', int32)) .or. &
            (c >= int(z'4E00', int32) .and. c <= int(z'9FFF', int32)) .or. &
            (c >= int(z'A000', int32) .and. c <= int(z'A4CF', int32)) .or. &
            (c >= int(z'AC00', int32) .and. c <= int(z'D7A3', int32)) .or. &
            (c >= int(z'F900', int32) .and. c <= int(z'FAFF', int32)) .or. &
            (c >= int(z'FE30', int32) .and. c <= int(z'FE4F', int32)) .or. &
            (c >= int(z'FF00', int32) .and. c <= int(z'FF60', int32)) .or. &
            (c >= int(z'FFE0', int32) .and. c <= int(z'FFE6', int32)) .or. &
            (c >= int(z'1F300', int32) .and. c <= int(z'1F64F', int32)) .or. &
            (c >= int(z'1F900', int32) .and. c <= int(z'1F9FF', int32)) .or. &
            (c >= int(z'20000', int32) .and. c <= int(z'3FFFD', int32))) glyph_width = 2
    end function glyph_width
end module character_mod
