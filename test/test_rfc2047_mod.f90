! test_rfc2047_mod.f90 -- tests for rfc2047_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/rfc2047-tests.el.
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

!> The rfc2047-tests.el tests of folding; own tests of decoding and
!> encoding.
module test_rfc2047_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point_minimum, point_maximum
    use bindings_mod, only: initialize_bindings
    use mime_utilities_mod, only: mime_coding_system_priorities
    use rfc2047_mod, only: fold_rfc2047_field, unfold_rfc2047_field, decode_rfc2047_string, &
                           decode_rfc2047_address_string, encode_rfc2047_string, encode_rfc2047_message_header, &
                           rfc2047_encoding_type, RFC2047_ENCODING_TYPE_MIME, RFC2047_ENCODING_TYPE_ADDRESS_MIME, &
                           pad_rfc2047_base64, rfc2047_field_value
    implicit none(type, external)
    private

    public :: collect_rfc2047_mod

    character(len=*), parameter :: LF = achar(10)
    ! "Grüße", "Köln" and "Jörg" in UTF-8.
    character(len=*), parameter :: GRUSSE = "Gr"//char(195)//char(188)//char(195)//char(159)//"e", &
                                   KOELN = "K"//char(195)//char(182)//"ln", JOERG = "J"//char(195)//char(182)//"rg"

contains

    !> Collect all exported unit tests.
    subroutine collect_rfc2047_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-rfc2047-fold-short", test_fold_short), &
                    new_unittest("test-rfc2047-fold-encoded", test_fold_encoded), &
                    new_unittest("test-rfc2047-fold-long", test_fold_long), &
                    new_unittest("test-rfc2047-fold-long-short", test_fold_long_short), &
                    new_unittest("rfc2047-unfold-field", test_unfold), &
                    new_unittest("rfc2047-decode-string", test_decode_string), &
                    new_unittest("rfc2047-decode-address-string", test_decode_address_string), &
                    new_unittest("rfc2047-encode-string", test_encode_string), &
                    new_unittest("rfc2047-encode-message-header", test_encode_message_header), &
                    new_unittest("rfc2047-pad-base64", test_pad_base64) &
                    ]
    end subroutine collect_rfc2047_mod

    !> (with-temp-buffer (insert TEXT) (goto-char (point-min))).
    subroutine temporary_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *rfc2047-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call goto_character(point_minimum())
    end subroutine temporary_buffer

    subroutine check_string(error, got, wanted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: got, wanted

        call check(error, got == wanted, "got """//got//""", wanted """//wanted//"""")
    end subroutine check_string

    ! Fold the field BEFORE and compare with AFTER.  Port of
    ! rfc2047-tests.el:test-rfc2047.
    subroutine expect_fold(error, before, after)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: before, after
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: end

        call temporary_buffer(before, h)
        call fold_rfc2047_field(end, st)
        call check_string(error, buffer_string(), after)
        call close_buffer(h, st)
    end subroutine expect_fold

    subroutine test_fold_short(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_fold(error, "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of Short Words Here"//LF, &
                         "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of"//LF// &
                         " Short Words Here"//LF)
    end subroutine test_fold_short

    subroutine test_fold_encoded(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_fold(error, "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's =?utf-8?Q?v=C3=A9ry?= long "// &
                         "and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it =?utf-8?Q?=C3=ADs?="//LF, &
                         "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's"//LF// &
                         " =?utf-8?Q?v=C3=A9ry?= long and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it"//LF// &
                         " =?utf-8?Q?=C3=ADs?="//LF)
    end subroutine test_fold_encoded

    subroutine test_fold_long(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_fold(error, "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword "// &
                         "and then"//LF, &
                         "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword"// &
                         LF//" and then"//LF)
    end subroutine test_fold_long

    subroutine test_fold_long_short(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_fold(error, "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword"// &
                         LF, "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword"// &
                         LF)
    end subroutine test_fold_long_short

    !> Unfolding joins the continuation lines that fit; the field value.
    subroutine test_unfold(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer("Subject: one"//LF//" two"//LF//"To: x"//LF, h)
        call unfold_rfc2047_field(st)
        call check_string(error, buffer_string(), "Subject: one two"//LF//"To: x"//LF)
        if (allocated(error)) return
        call check_string(error, rfc2047_field_value(st), "one two"//LF)
        call close_buffer(h, st)
    end subroutine test_unfold

    subroutine test_decode_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, decode_rfc2047_string("=?utf-8?Q?Gr=C3=BC=C3=9Fe?="), GRUSSE)
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_string("=?UTF-8?q?Gr=C3=BC=C3=9Fe?= aus K=F6ln"), GRUSSE//" aus K=F6ln")
        if (allocated(error)) return
        ! Successive encoded words are joined, even within a character.
        call check_string(error, decode_rfc2047_string("=?utf-8?B?R3LDvA==?= =?utf-8?B?w59l?="), GRUSSE)
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_string("Re: =?iso-8859-1?q?K=F6ln?="), "Re: "//KOELN)
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_string("=?utf-8?Q?a_b?="), "a b")
        if (allocated(error)) return
        ! A charset nothing decodes leaves the word alone.
        call check_string(error, decode_rfc2047_string("=?x-unknown?Q?abc?="), "=?x-unknown?Q?abc?=")
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_string("plain text"), "plain text")
    end subroutine test_decode_string

    subroutine test_decode_address_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, decode_rfc2047_address_string("""Foo \Bar"" <a@b>"), """Foo Bar"" <a@b>")
        if (allocated(error)) return
        ! Encoded words are decoded in quoted strings too.
        call check_string(error, decode_rfc2047_address_string("""=?utf-8?Q?J=C3=B6rg?="" <j@example.org>"), &
                          """"//JOERG//""" <j@example.org>")
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_address_string("=?utf-8?Q?J=C3=B6rg?= <j@example.org>"), &
                          JOERG//" <j@example.org>")
    end subroutine test_decode_address_string

    subroutine test_encode_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        mime_coding_system_priorities = [character(len=24) :: "utf-8"]
        call check_string(error, encode_rfc2047_string(GRUSSE, st), "=?utf-8?B?R3LDvMOfZQ==?=")
        if (allocated(error)) return
        call check_string(error, encode_rfc2047_string("plain text", st), "plain text")
        if (allocated(error)) return
        rfc2047_encoding_type = RFC2047_ENCODING_TYPE_MIME
        call check_string(error, encode_rfc2047_string("Re: "//GRUSSE//" aus "//KOELN, st), &
                          "Re: =?utf-8?B?R3LDvMOfZQ==?= aus =?utf-8?Q?K=C3=B6ln?=")
        rfc2047_encoding_type = RFC2047_ENCODING_TYPE_ADDRESS_MIME
        if (allocated(error)) return
        call check_string(error, encode_rfc2047_string(JOERG//" <j@example.org>", st), &
                          "=?utf-8?Q?J=C3=B6rg?= <j@example.org>")
        if (allocated(error)) return
        call check_string(error, encode_rfc2047_string(""""//JOERG//" X"" <j@example.org>", st), &
                          "=?utf-8?Q?J=C3=B6rg_X?= <j@example.org>")
        if (allocated(error)) return
        call check_string(error, decode_rfc2047_string(encode_rfc2047_string(JOERG//" (Mr. "//KOELN//") <j@x.org>", &
                                                                             st)), JOERG//" (Mr. "//KOELN//") <j@x.org>")
    end subroutine test_encode_string

    subroutine test_encode_message_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        mime_coding_system_priorities = [character(len=24) :: "utf-8"]
        call temporary_buffer("Subject: "//GRUSSE//LF//"To: "//JOERG//" <j@example.org>"//LF// &
                              "Message-ID: <1@example.org>"//LF, h)
        call encode_rfc2047_message_header(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check_string(error, buffer_string(), "Subject: =?utf-8?B?R3LDvMOfZQ==?="//LF// &
                          "To: =?utf-8?Q?J=C3=B6rg?= <j@example.org>"//LF//"Message-ID: <1@example.org>"//LF)
        call close_buffer(h, st)
    end subroutine test_encode_message_header

    subroutine test_pad_base64(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, pad_rfc2047_base64("R3LDvA"), "R3LDvA==")
        if (allocated(error)) return
        call check_string(error, pad_rfc2047_base64("R3LDvMOfZQ="), "R3LDvMOfZQ==")
        if (allocated(error)) return
        call check_string(error, pad_rfc2047_base64("abcd"), "abcd")
        if (allocated(error)) return
        call check_string(error, pad_rfc2047_base64("abcde"), "abcde")
    end subroutine test_pad_base64
end module test_rfc2047_mod
