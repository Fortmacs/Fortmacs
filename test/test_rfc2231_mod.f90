! test_rfc2231_mod.f90 -- tests for rfc2231_mod.
! Own tests; upstream has no rfc2231 tests.
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

!> Parsing MIME parameters: plain, quoted, continued and encoded values,
!> comments, and an invalid parameter list.
module test_rfc2231_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use bindings_mod, only: initialize_bindings
    use rfc2231_mod, only: mime_header_value_type, parse_rfc2231_string, get_rfc2231_value, decode_rfc2231_encoded_string
    implicit none(type, external)
    private

    public :: collect_rfc2231_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_rfc2231_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rfc2231-parse-string", test_parse), &
                    new_unittest("rfc2231-parse-continuations", test_continuations), &
                    new_unittest("rfc2231-decode-encoded-string", test_decode), &
                    new_unittest("rfc2231-parse-invalid", test_invalid) &
                    ]
    end subroutine collect_rfc2231_mod

    !> The name and the parameters, in lower case where upstream downcases.
    subroutine test_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(mime_header_value_type) :: header
        character(len=:), allocatable :: value
        type(status_type) :: st

        call parse_rfc2231_string('Text/Plain (a comment); Charset="UTF-8"; format=flowed', header, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, header%name == "text/plain", header%name)
        if (allocated(error)) return
        call get_rfc2231_value(header, "charset", value)
        call check(error, allocated(value))
        if (allocated(error)) return
        call check(error, value == "UTF-8", value)
        if (allocated(error)) return
        call get_rfc2231_value(header, "format", value)
        call check(error, value == "flowed", value)
        if (allocated(error)) return
        call get_rfc2231_value(header, "boundary", value)
        call check(error,.not. allocated(value))
        if (allocated(error)) return
        ! Upstream lists the parameters from the last to the first.
        call check(error, header%parameters(1)%attribute == "format")
    end subroutine test_parse

    !> Continued values are joined in the order of their numbers, and an
    !> encoded value is decoded by its charset.
    subroutine test_continuations(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: ETE = char(195)//char(169)//"t"//char(195)//char(169)
        type(mime_header_value_type) :: header
        character(len=:), allocatable :: value
        type(status_type) :: st

        call initialize_bindings()
        call parse_rfc2231_string('attachment; filename*1="name.txt"; filename*0="long"', header, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call get_rfc2231_value(header, "filename", value)
        call check(error, value == "longname.txt", value)
        if (allocated(error)) return
        call parse_rfc2231_string("attachment; filename*=utf-8''%C3%A9t%C3%A9.txt", header, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call get_rfc2231_value(header, "filename", value)
        call check(error, value == ETE//".txt", value)
    end subroutine test_continuations

    !> The forms of rfc2231-decode-encoded-string's docstring.
    subroutine test_decode(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: decoded
        type(status_type) :: st

        call initialize_bindings()
        call decode_rfc2231_encoded_string("us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A", decoded, st)
        call check(error, decoded == "This is ***fun***", decoded)
        if (allocated(error)) return
        call decode_rfc2231_encoded_string("''This%20is%20%2A%2A%2Afun%2A%2A%2A", decoded, st)
        call check(error, decoded == "This is ***fun***", decoded)
        if (allocated(error)) return
        call decode_rfc2231_encoded_string("This is ***fun***", decoded, st)
        call check(error, decoded == "This is ***fun***", decoded)
        if (allocated(error)) return
        call decode_rfc2231_encoded_string("", decoded, st)
        call check(error,.not. st%ok())
    end subroutine test_decode

    !> Parameters that cannot be parsed are left out, unless an error is
    !> wanted.
    subroutine test_invalid(error)
        type(error_type), allocatable, intent(out) :: error
        type(mime_header_value_type) :: header
        type(status_type) :: st

        call parse_rfc2231_string("text/plain; charset=utf-8; =x", header, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, header%name == "text/plain", header%name)
        if (allocated(error)) return
        call check(error, size(header%parameters) == 0)
        if (allocated(error)) return
        call parse_rfc2231_string("text/plain; =x", header, st, report_error=.true.)
        call check(error,.not. st%ok())
    end subroutine test_invalid
end module test_rfc2231_mod
