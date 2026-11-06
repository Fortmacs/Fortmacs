! rfc2231_mod.f90 -- functions for decoding RFC 2231 MIME parameters.
! Ported from GNU Emacs 31.1 lisp/mail/rfc2231.el (in part: see
! docs/status/rfc2231_mod.md).
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

!> RFC 2231 is "MIME Parameter Value and Encoded Word Extensions": the
!> parameters of header fields such as Content-Type and
!> Content-Disposition, `text/plain; charset=utf-8', with values continued
!> over several parameters (`name*0=', `name*1=') and encoded
!> (`name*=utf-8''%C3%A9t%C3%A9').  `parse_rfc2231_string' parses such a
!> field's value into an `rfc2231_header_type': the value before the
!> parameters (`name') and the parameters, each an attribute and its value,
!> read with `get_rfc2231_value'.  mail-parse.el's names
!> (mail-header-parse-content-type, mail-header-parse-content-disposition,
!> mail-content-type-get) are aliases, collapsed to these names.  Upstream
!> scans a temporary buffer with a syntax table derived from
!> `ietf-drums-syntax-table'; here the value is scanned as a string with
!> the same rules.  What is not ported yet is listed in
!> docs/status/rfc2231_mod.md.
module rfc2231_mod
    use, intrinsic :: iso_fortran_env, only: int32
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use character_mod, only: downcase_ascii, string_to_multibyte
    use coding_mod, only: decode_coding_string
    use ietf_drums_mod, only: remove_ietf_drums_comments, remove_ietf_drums_whitespace, ietf_drums_string_end, &
                              ietf_drums_comment_end, IETF_DRUMS_TSPECIALS
    use rfc2047_mod, only: decode_rfc2047_string
    use mime_utilities_mod, only: mime_character_set_to_coding_system
    implicit none(type, external)
    private

    public :: parse_rfc2231_string, parse_rfc2231_quoted_printable_string, get_rfc2231_value, &
              decode_rfc2231_encoded_string

    !> One parameter of a header field: its ATTRIBUTE (in lower case) and
    !> its VALUE.  Not a port: an element of upstream's parsed list.
    type, public :: rfc2231_parameter_type
        !> The attribute's name, in lower case.
        character(len=:), allocatable :: attribute
        !> The value, decoded.
        character(len=:), allocatable :: value
    end type rfc2231_parameter_type

    !> A parsed header field value: the NAME before the parameters (such as
    !> "text/plain" or "attachment", in lower case; unallocated when the
    !> value does not start with a token) and the PARAMETERS.
    !> Not a port: upstream's parsed list, the name and two values for
    !> each parameter.
    type, public :: rfc2231_header_type
        !> The value before the parameters, in lower case.
        character(len=:), allocatable :: name
        !> The parameters, in the order upstream lists them.
        type(rfc2231_parameter_type), allocatable :: parameters(:)
    end type rfc2231_header_type

    ! A parameter as it is read, before continuations are joined.
    type :: raw_parameter_type
        character(len=:), allocatable :: attribute, value
        ! The part number (`name*N'), -1 for none.
        integer :: number = -1
        logical :: encoded = .false.
    end type raw_parameter_type

    ! A joined parameter, before its value is decoded.
    type :: joined_parameter_type
        character(len=:), allocatable :: attribute, value
        logical :: encoded = .false.
    end type joined_parameter_type

    character(len=*), parameter :: LF = achar(10), CR = achar(13), TAB = achar(9), FORM_FEED = achar(12)

contains

    ! ------------------------------------------------------------------
    ! The syntax of parameters
    ! ------------------------------------------------------------------

    ! Whether C is in ietf-drums.el's ietf-drums-text-token (ASCII but NUL,
    ! newline and carriage return).  Not a port.
    pure logical function is_text_token(c)
        character, intent(in) :: c

        is_text_token = iachar(c) >= 1 .and. iachar(c) <= 127 .and. c /= LF .and. c /= CR
    end function is_text_token

    ! Whether C has whitespace syntax in the table rfc2231-parse-string
    ! scans with (`*', `;' and `=' are made whitespace).  Not a port.
    pure logical function is_whitespace_syntax(c)
        character, intent(in) :: c

        is_whitespace_syntax = scan(c, " "//TAB//FORM_FEED//"*;=") > 0
    end function is_whitespace_syntax

    ! The position after the expression that forward-sexp moves over from
    ! position START of STRING, in the table rfc2231-parse-string scans
    ! with; 0 for a scan error (an unterminated string or group).
    ! Not a port: forward-sexp with that table, on a string.
    pure integer function expression_end(string, start) result(end)
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer :: q

        q = start

        do while (q <= len(string))
            if (.not. is_whitespace_syntax(string(q:q)) .and. string(q:q) /= LF) exit
            q = q + 1
        end do

        end = q
        if (q > len(string)) return

        select case (string(q:q))
        case ('"')
            end = ietf_drums_string_end(string, q)
            if (end > 0) end = end + 1
        case ("(", "[", "<")
            end = ietf_drums_comment_end(string, q)
            if (end > len(string)) end = 0
        case (")", "]", ">")
            end = 0
        case default
            do while (q <= len(string))
                if (string(q:q) == "\") then
                    q = q + 2
                    cycle
                end if

                if (is_whitespace_syntax(string(q:q)) .or. scan(string(q:q), LF//'"()[]<>') > 0) exit
                q = q + 1
            end do

            end = min(q, len(string) + 1)
        end select
    end function expression_end

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> Return the value of ATTRIBUTE from HEADER in VALUE (unallocated if
    !> HEADER has no such parameter).  Port of rfc2231.el:rfc2231-get-value
    !> (upstream's mail-content-type-get).
    subroutine get_rfc2231_value(header, attribute, value)
        type(rfc2231_header_type), intent(in) :: header
        character(len=*), intent(in) :: attribute
        character(len=:), allocatable, intent(out) :: value
        integer :: k

        if (.not. allocated(header%parameters)) return

        do k = 1, size(header%parameters)
            if (header%parameters(k)%attribute == attribute) then
                value = header%parameters(k)%value
                return
            end if
        end do
    end subroutine get_rfc2231_value

    !> Parse QP-encoded STRING using `parse_rfc2231_string'.
    !> N.B.  This is in violation with RFC2047, but it seem to be in common use.
    !> Port of rfc2231.el:rfc2231-parse-qp-string (upstream's
    !> mail-header-parse-content-type and
    !> mail-header-parse-content-disposition).
    subroutine parse_rfc2231_quoted_printable_string(string, header, status)
        character(len=*), intent(in) :: string
        type(rfc2231_header_type), intent(out) :: header
        type(status_type), intent(out) :: status

        call parse_rfc2231_string(decode_rfc2047_string(string), header, status)
    end subroutine parse_rfc2231_quoted_printable_string

    !> Parse STRING into HEADER: its name, then its parameters, each an
    !> attribute and its value.
    !>
    !> If REPORT_ERROR is true, an error condition occurs when this
    !> function fails in parsing of parameters.  Otherwise, an error
    !> condition occurs only for an encoded value it cannot decode (see
    !> `decode_rfc2231_encoded_string'), and parameters it cannot parse are
    !> left out.  Port of rfc2231.el:rfc2231-parse-string (removing comments
    !> and whitespace cannot fail here, so its fallback for an error there
    !> is not needed).
    subroutine parse_rfc2231_string(string, header, status, report_error)
        character(len=*), intent(in) :: string
        type(rfc2231_header_type), intent(out) :: header
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: report_error
        character(len=:), allocatable :: text, attribute, value
        type(raw_parameter_type), allocatable :: parameters(:), sorted(:)
        type(raw_parameter_type) :: raw
        type(joined_parameter_type), allocatable :: joined(:)
        integer :: p, q, number, k, m, element, used
        logical :: encoded, failed

        status = status_type()
        allocate (header%parameters(0))
        text = remove_ietf_drums_whitespace(remove_ietf_drums_comments(string))
        p = 1
        if (len(text) == 0) return
        if (.not. is_text_token(text(1:1)) .or. index(IETF_DRUMS_TSPECIALS, text(1:1)) > 0) return
        q = expression_end(text, p)
        if (q == 0) return
        header%name = downcase_ascii(text(p:q - 1))
        p = q
        ! Do the parameters.
        allocate (parameters(0))
        failed = .false.

        parameter_loop: do while (p <= len(text))
            if (text(p:p) /= ";") then
                failed = .true.
                exit parameter_loop
            end if

            p = p + 1
            ! If there is no character, then this is an invalid header, but
            ! since elm generates invalid headers on this form, we allow it.
            if (p > len(text)) exit parameter_loop

            if (.not. is_text_token(text(p:p)) .or. index(IETF_DRUMS_TSPECIALS, text(p:p)) > 0) then
                failed = .true.
                exit parameter_loop
            end if

            q = expression_end(text, p)

            if (q == 0) then
                failed = .true.
                exit parameter_loop
            end if

            attribute = downcase_ascii(text(p:q - 1))
            p = q
            number = -1
            encoded = .false.

            if (p <= len(text)) then
                if (text(p:p) == "*") then
                    p = p + 1
                    encoded = .true.

                    if (p <= len(text)) then
                        if (verify(text(p:p), "0123456789") == 0) then
                            encoded = .false.
                            q = expression_end(text, p)

                            if (q == 0) then
                                failed = .true.
                                exit parameter_loop
                            end if

                            number = leading_number(text(p:q - 1))
                            p = q

                            if (p <= len(text)) then
                                if (text(p:p) == "*") then
                                    encoded = .true.
                                    p = p + 1
                                end if
                            end if
                        end if
                    end if
                end if
            end if

            if (p > len(text)) then
                failed = .true.
                exit parameter_loop
            end if

            if (text(p:p) /= "=") then
                failed = .true.
                exit parameter_loop
            end if

            p = p + 1

            if (p > len(text)) then
                failed = .true.
                exit parameter_loop
            end if

            if (text(p:p) == '"') then
                q = ietf_drums_string_end(text, p)

                if (q == 0) then
                    failed = .true.
                    exit parameter_loop
                end if

                value = text(p + 1:q - 1)
                p = q + 1
                if (encoded) value = percent_encode(value)
            else if ((is_text_token(text(p:p)) .or. iachar(text(p:p)) > 127) .and. &
                     index(IETF_DRUMS_TSPECIALS, text(p:p)) == 0) then
                ! Jump over asterisk, non-ASCII and non-boundary characters.
                q = p

                do while (q <= len(text))
                    if (text(q:q) /= "*" .and. iachar(text(q:q)) <= 127 .and. is_whitespace_syntax(text(q:q))) exit
                    q = q + 1
                end do

                value = text(p:q - 1)
                p = q
            else
                failed = .true.
                exit parameter_loop
            end if

            raw = raw_parameter_type(attribute, value, number, encoded)
            parameters = [parameters, raw]
        end do parameter_loop

        if (failed) then
            deallocate (parameters)
            allocate (parameters(0))

            if (present(report_error)) then
                if (report_error) then
                    call set_error(status, GENERIC_ERROR_CODE, "Invalid header: "//string)
                    return
                end if
            end if
        end if

        ! Now collect and concatenate continuation parameters, in the order
        ! upstream visits them: the parameters from the last to the first,
        ! stably sorted by their part numbers.
        allocate (sorted(0))

        do m = 0, maxval([0, parameters%number])
            do k = size(parameters), 1, -1
                if (max(parameters(k)%number, 0) == m) sorted = [sorted, parameters(k)]
            end do
        end do

        allocate (joined(size(sorted)))
        used = 0

        do k = 1, size(sorted)
            element = 0

            do m = used, 1, -1
                if (joined(m)%attribute == sorted(k)%attribute) then
                    element = m
                    exit
                end if
            end do

            if (element == 0 .or. sorted(k)%number == 0) then
                ! First part.
                used = used + 1
                joined(used)%attribute = sorted(k)%attribute
                joined(used)%value = sorted(k)%value
                joined(used)%encoded = sorted(k)%encoded
            else if (sorted(k)%number >= 0) then
                ! Concatenate continuation parts (a repeated part without a
                ! number is left out).
                joined(element)%value = joined(element)%value//sorted(k)%value
            end if
        end do

        ! Finally decode encoded values.
        deallocate (header%parameters)
        allocate (header%parameters(used))

        do k = 1, used
            header%parameters(k)%attribute = joined(k)%attribute

            if (joined(k)%encoded) then
                call decode_rfc2231_encoded_string(joined(k)%value, header%parameters(k)%value, status)
                if (.not. status%ok()) return
            else
                header%parameters(k)%value = joined(k)%value
            end if
        end do

    contains

        ! The number the digits at the start of DIGITS make (string-to-number).
        pure integer function leading_number(digits) result(n)
            character(len=*), intent(in) :: digits
            integer :: i

            n = 0

            do i = 1, len(digits)
                if (verify(digits(i:i), "0123456789") /= 0) exit
                n = 10*n + (iachar(digits(i:i)) - iachar("0"))
            end do
        end function leading_number

        ! Each byte of TEXT as `%xx'.
        pure function percent_encode(text) result(encoded)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: encoded
            integer :: i

            allocate (character(len=3*len(text)) :: encoded)

            do i = 1, len(text)
                write (encoded(3*i - 2:3*i), "(a, z2.2)") "%", iachar(text(i:i))
            end do

            encoded = downcase_ascii(encoded)
        end function percent_encode
    end subroutine parse_rfc2231_string

    !> Decode an RFC2231-encoded string.
    !> These look like:
    !>  "us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A",
    !>  "us-ascii''This%20is%20%2A%2A%2Afun%2A%2A%2A",
    !>  "'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A",
    !>  "''This%20is%20%2A%2A%2Afun%2A%2A%2A", or
    !>  "This is ***fun***".
    !> Port of rfc2231.el:rfc2231-decode-encoded-string.
    subroutine decode_rfc2231_encoded_string(string, decoded, status)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: decoded
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: character_set, value, bytes, coding_system
        integer :: first, second, i, byte, iostat

        status = status_type()
        character_set = ""
        value = ""
        first = index(string, "'")
        second = 0
        if (first > 0) second = index(string(first + 1:), "'")

        if (second > 0) then
            second = first + second
            value = string(second + 1:)
            character_set = string(1:first - 1)
        end if

        if (len(value) == 0 .or. index(value, LF) > 0) then
            character_set = ""
            value = string
        end if

        if (len(value) == 0 .or. index(value, LF) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, 'Unrecognized RFC2231 format: "'//string//'"')
            return
        end if

        bytes = ""
        i = 1

        do while (i <= len(value))
            if (value(i:i) == "%" .and. i + 2 <= len(value)) then
                if (verify(value(i + 1:i + 2), "0123456789abcdefABCDEF") == 0) then
                    read (value(i + 1:i + 2), "(z2)", iostat=iostat) byte

                    if (iostat == 0) then
                        bytes = bytes//char(byte)
                        i = i + 3
                        cycle
                    end if
                end if
            end if

            bytes = bytes//value(i:i)
            i = i + 1
        end do

        coding_system = ""
        if (len(character_set) > 0) coding_system = mime_character_set_to_coding_system(character_set, allow_override=.true.)

        ! Decode using the charset, if any.
        if (len(coding_system) == 0 .or. coding_system == "ascii") then
            decoded = string_to_multibyte(bytes)
        else
            call decode_coding_string(bytes, coding_system, decoded, status, .false.)
        end if
    end subroutine decode_rfc2231_encoded_string
end module rfc2231_mod
