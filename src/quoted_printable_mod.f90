! quoted_printable_mod.f90 -- Quoted-Printable functions.
! Ported from GNU Emacs 31.1 lisp/mail/qp.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> Functions for encoding and decoding quoted-printable text as defined in
!> RFC 2045: `decode_quoted_printable_region', `encode_quoted_printable_region'
!> and their string forms.  What is not ported yet is listed in
!> docs/status/quoted_printable_mod.md.
module quoted_printable_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name
    use editing_functions_mod, only: point, point_min, point_max, goto_char, narrow_to_region, char_after, is_eob, &
                                     is_eol, delete_region, buffer_string, line_beginning_position, interactive_region, &
                                     save_excursion_begin, save_excursion_end, save_restriction_begin, &
                                     save_restriction_end, excursion_type, restriction_type
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_char, forward_line, end_of_line, beginning_of_line, delete_char
    use syntax_mod, only: skip_chars_forward
    use search_mod, only: re_search_forward, search_backward, looking_at, match_beginning, match_end, replace_match, &
                          case_fold_search, SEARCH_NOERROR
    use character_mod, only: is_char_byte8, char_to_byte8, str_to_multibyte
    use coding_mod, only: is_coding_system, encode_coding_region, decode_coding_region, coding_system_for_read
    use indent_mod, only: current_column
    use display_engine_mod, only: tab_width, message
    implicit none(type, external)
    private

    public :: decode_quoted_printable_region, decode_quoted_printable_string, encode_quoted_printable_region, &
              encode_quoted_printable_string, init_quoted_printable

    character(len=*), parameter :: LF = achar(10)
    character(len=*), parameter :: HEX_DIGITS = "0123456789ABCDEF"

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Make a temporary buffer current (with-temp-buffer), unibyte unless
    ! MULTIBYTE; OLD is the buffer to go back to.  Not a port.
    subroutine temporary_buffer_begin(old, temporary, multibyte)
        type(handle_type), intent(out) :: old, temporary
        logical, intent(in) :: multibyte
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        buffers(current)%multibyte = multibyte
    end subroutine temporary_buffer_begin

    ! Close the temporary buffer TEMPORARY and make OLD current again.
    ! Not a port.
    subroutine temporary_buffer_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_buffer_end

    ! The value of the hexadecimal digit C (either case).  Not a port.
    integer function hex_value(c)
        integer(int32), intent(in) :: c

        if (c <= iachar("9")) then
            hex_value = c - iachar("0")
        else if (c <= iachar("F")) then
            hex_value = c - iachar("A") + 10
        else
            hex_value = c - iachar("a") + 10
        end if
    end function hex_value

    ! The byte of the character after point (get-byte): the character
    ! itself if ASCII or in a unibyte buffer, its byte if a raw byte.
    ! Not a port.
    integer function byte_after()
        integer(int32) :: c
        logical :: found

        call char_after(c, found)
        byte_after = c
        if (is_char_byte8(c)) byte_after = char_to_byte8(c)
    end function byte_after

    ! Replace the character after point by "=XX", its byte in hexadecimal.
    ! Not a port.
    subroutine encode_byte_after(status)
        type(status_type), intent(out) :: status
        integer :: byte

        byte = byte_after()
        call insert("="//HEX_DIGITS(byte/16 + 1:byte/16 + 1)//HEX_DIGITS(mod(byte, 16) + 1:mod(byte, 16) + 1), status)
        if (status%ok()) call delete_char(1_int64, status)
    end subroutine encode_byte_after

    ! ------------------------------------------------------------------
    ! Decoding
    ! ------------------------------------------------------------------

    !> Decode quoted-printable in the region between FROM and TO, per RFC
    !> 2045.  If CODING_SYSTEM is present and a coding system, decode bytes
    !> into characters with that coding system.
    !>
    !> Interactively, you can supply the CODING_SYSTEM argument with
    !> \[universal_coding_system_argument].
    !>
    !> The CODING_SYSTEM argument is a historical hangover and is
    !> deprecated.  QP encodes raw bytes and should be decoded into raw
    !> bytes.  Decoding them into characters should be done separately.
    !> Port of qp.el:quoted-printable-decode-region.
    recursive subroutine decode_quoted_printable_region(from, to, status, coding_system)
        integer(int64), intent(in) :: from, to
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: coding_system
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: coding, bytes
        integer(int64) :: moved, inserted, n, k, beginning, end
        integer(int32) :: n1, n2
        logical :: saved_fold, matched, found

        coding = ""

        if (present(coding_system)) then
            ! e.g. `ascii' from Gnus
            if (is_coding_system(coding_system)) coding = coding_system
        end if

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        ! RFC 2045:  ``An "=" followed by two hexadecimal digits, one or
        ! both of which are lowercase letters in "abcdef", is formally
        ! illegal. A robust implementation might choose to recognize them
        ! as the corresponding uppercase letters.''
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.

        body: block
            call narrow_to_region(from, to, status)
            if (.not. status%ok()) exit body

            ! Do this in case we're called from Gnus, say, in a buffer which
            ! already contains non-ASCII characters which would then get
            ! doubly-decoded below.
            if (len(coding) > 0) then
                call encode_coding_region(point_min(), point_max(), coding, inserted, status)
                if (.not. status%ok()) exit body
            end if

            call goto_char(point_min())

            do
                moved = skip_chars_forward("^=", status)
                if (.not. status%ok()) exit body
                if (is_eob()) exit
                call char_after(n1, found, point() + 1)

                if (found) then
                    if (n1 == 10) then
                        call delete_char(2_int64, status)
                        if (.not. status%ok()) exit body
                        cycle
                    end if
                end if

                matched = looking_at("\(=[0-9A-F][0-9A-F]\)+", status)
                if (.not. status%ok()) exit body

                if (.not. matched) then
                    call message("Malformed quoted-printable text")
                    call forward_char(status)
                    if (.not. status%ok()) exit body
                    cycle
                end if

                ! Decode this sequence at once; i.e. by a single deletion
                ! and insertion.
                beginning = match_beginning(0, status)
                end = match_end(0, status)
                n = (end - point())/3
                bytes = repeat(" ", int(n))

                do k = 1, n
                    call char_after(n1, found, point() + 1)
                    call char_after(n2, found, point() + 2)
                    bytes(k:k) = achar(16*hex_value(n1) + hex_value(n2))
                    call forward_char(status, 3_int64)
                end do

                call delete_region(beginning, end, status)

                if (status%ok()) then
                    if (buffers(current)%multibyte) then
                        call insert(str_to_multibyte(bytes), status)
                    else
                        call insert(bytes, status)
                    end if
                end if

                if (.not. status%ok()) exit body
            end do

            if (len(coding) > 0) call decode_coding_region(point_min(), point_max(), coding, inserted, status)
        end block body

        case_fold_search%value = saved_fold
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine decode_quoted_printable_region

    !> Decode the quoted-printable encoded STRING (bytes) and return the
    !> result.  If CODING_SYSTEM is present, decode the string with that
    !> coding system.  Use of CODING_SYSTEM is deprecated; this function
    !> should deal with raw bytes, and coding conversion should be done
    !> separately.  Port of qp.el:quoted-printable-decode-string.
    function decode_quoted_printable_string(string, coding_system, status) result(decoded)
        character(len=*), intent(in) :: string
        character(len=*), intent(in), optional :: coding_system
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: decoded
        type(handle_type) :: old, temporary
        type(status_type) :: st

        call temporary_buffer_begin(old, temporary, .false.)
        call insert(string, st)
        if (st%ok()) call decode_quoted_printable_region(point_min(), point_max(), st, coding_system)
        decoded = buffer_string()
        call temporary_buffer_end(old, temporary)
        if (present(status)) status = st
    end function decode_quoted_printable_string

    ! ------------------------------------------------------------------
    ! Encoding
    ! ------------------------------------------------------------------

    !> Quoted-printable encode the region between FROM and TO per RFC 2045.
    !>
    !> If FOLD, fold long lines at 76 characters (as required by the RFC).
    !> If CLASS is present, translate the characters not matched by that
    !> regular expression class, which is in the form expected by
    !> `skip_chars_forward'.  You should probably avoid non-ASCII
    !> characters in this argument.  A character in the region that is
    !> neither ASCII nor a raw byte is the error "Multibyte character in QP
    !> encoding region" in STATUS.  Port of
    !> qp.el:quoted-printable-encode-region.
    recursive subroutine encode_quoted_printable_region(from, to, status, fold, class)
        integer(int64), intent(in) :: from, to
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fold
        character(len=*), intent(in), optional :: class
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: chars
        integer(int64) :: moved, found, shortage, saved_tab_width, p
        integer(int32) :: c
        logical :: saved_fold, folding, exists

        if (present(class)) then
            chars = class
        else
            ! Avoid using 8bit characters. = is \075.  Equivalent to
            ! "^\000-\007\013\015-\037\200-\377="
            chars = achar(8)//"-"//achar(10)//achar(12)//achar(32)//"-"//achar(60)//achar(62)//"-"//achar(127)
        end if

        folding = .false.
        if (present(fold)) folding = fold
        call save_excursion_begin(excursion)

        body: block
            do p = from, to - 1
                call char_after(c, exists, p)
                if (.not. buffers(current)%multibyte) exit
                if (c <= 127) cycle

                if (.not. is_char_byte8(c)) then
                    call set_error(status, FM_GENERIC_ERROR, "Multibyte character in QP encoding region")
                    exit body
                end if
            end do

            call save_restriction_begin(restriction)
            call narrow_to_region(from, to, status)

            narrowed: block
                if (.not. status%ok()) exit narrowed
                ! Encode all the non-ascii and control characters.
                call goto_char(point_min())

                do
                    moved = skip_chars_forward(chars, status)
                    if (.not. status%ok()) exit narrowed
                    if (is_eob()) exit
                    call encode_byte_after(status)
                    if (.not. status%ok()) exit narrowed
                end do

                ! Encode white space at the end of lines.
                call goto_char(point_min())

                do
                    call re_search_forward("[ "//achar(9)//"]+$", found, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) exit narrowed
                    if (found == 0) exit
                    call goto_char(match_beginning(0, status))

                    do while (.not. is_eol())
                        call encode_byte_after(status)
                        if (.not. status%ok()) exit narrowed
                    end do
                end do

                if (.not. folding) exit narrowed
                ! HTAB is one character.
                saved_tab_width = tab_width%value
                tab_width%value = 1
                saved_fold = case_fold_search%value
                case_fold_search%value = .false.
                call goto_char(point_min())

                do while (.not. is_eob())
                    call end_of_line()

                    ! Fold long lines.
                    do while (current_column() > 76)
                        call beginning_of_line()
                        ! 75 chars plus an "="
                        call forward_char(status, 75_int64)
                        if (.not. status%ok()) exit
                        call search_backward("=", found, status, bound=point() - 2, noerror=SEARCH_NOERROR)
                        if (.not. status%ok()) exit
                        call insert("="//LF, status)
                        if (.not. status%ok()) exit
                        call end_of_line()
                    end do

                    if (.not. status%ok()) exit
                    shortage = forward_line(1_int64)
                end do

                tab_width%value = saved_tab_width
                case_fold_search%value = saved_fold
            end block narrowed

            call save_restriction_end(restriction)
        end block body

        call save_excursion_end(excursion)
    end subroutine encode_quoted_printable_region

    !> Encode the STRING as quoted-printable and return the result: a
    !> string of bytes, unless MULTIBYTE (absent: false) says it holds
    !> multibyte text, whose characters other than ASCII and raw bytes are
    !> the error of `encode_quoted_printable_region' in STATUS.  Port of
    !> qp.el:quoted-printable-encode-string.
    function encode_quoted_printable_string(string, multibyte, status) result(encoded)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: multibyte
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: encoded
        type(handle_type) :: old, temporary
        type(status_type) :: st
        logical :: is_multibyte

        is_multibyte = .false.
        if (present(multibyte)) is_multibyte = multibyte
        call temporary_buffer_begin(old, temporary, is_multibyte)
        call insert(string, st)
        if (st%ok()) call encode_quoted_printable_region(point_min(), point_max(), st)
        encoded = buffer_string()
        call temporary_buffer_end(old, temporary)
        if (present(status)) status = st
    end function encode_quoted_printable_string

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_decode_quoted_printable_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        ! Let the user determine the coding system with "C-x RET c".
        call interactive_region(b, e, status)
        if (.not. status%ok()) return

        if (allocated(coding_system_for_read%value)) then
            call decode_quoted_printable_region(b, e, status, coding_system_for_read%value)
        else
            call decode_quoted_printable_region(b, e, status)
        end if
    end subroutine cmd_decode_quoted_printable_region

    recursive subroutine cmd_encode_quoted_printable_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (status%ok()) call encode_quoted_printable_region(b, e, status)
    end subroutine cmd_encode_quoted_printable_region

    !> Register the quoted-printable region commands.  Not a port.
    subroutine init_quoted_printable()
        commands%decode_quoted_printable_region%execute => cmd_decode_quoted_printable_region
        commands%encode_quoted_printable_region%execute => cmd_encode_quoted_printable_region
    end subroutine init_quoted_printable
end module quoted_printable_mod
