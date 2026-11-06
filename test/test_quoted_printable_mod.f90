! test_quoted_printable_mod.f90 -- tests for quoted_printable_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/qp-tests.el.
!
! Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

!> The qp-tests.el tests, with the quote by Antoine de Saint-Exupery,
!> Citadelle (1948), from https://en.wikipedia.org/wiki/Quoted-printable.
module test_quoted_printable_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point_minimum, point_maximum
    use bindings_mod, only: initialize_bindings
    use coding_mod, only: encode_coding_region
    use quoted_printable_mod, only: decode_quoted_printable_region, decode_quoted_printable_string, &
                                    encode_quoted_printable_region, encode_quoted_printable_string
    implicit none(type, external)
    private

    public :: collect_quoted_printable_mod

    character(len=*), parameter :: LF = achar(10)
    ! UTF-8 of e acute, a grave, o circumflex and a circumflex.
    character(len=*), parameter :: E_ACUTE = char(195)//char(169), A_GRAVE = char(195)//char(160), &
                                   O_CIRCUMFLEX = char(195)//char(180), A_CIRCUMFLEX = char(195)//char(162)
    character(len=*), parameter :: QUOTE_QP = &
                                   "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font ="//LF// &
                                   "vite p=C3=A9dagogues et t'enseignent comme but ce qui n'est par essence qu'="//LF// &
                                   "un moyen, et te trompant ainsi sur la route =C3=A0 suivre les voil=C3=A0 bi="//LF// &
                                   "ent=C3=B4t qui te d=C3=A9gradent, car si leur musique est vulgaire ils te f="//LF// &
                                   "abriquent pour te la vendre une =C3=A2me vulgaire."
    character(len=*), parameter :: QUOTE_UTF8 = &
                                   "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font "// &
                                   "vite p"//E_ACUTE//"dagogues et t'enseignent comme but ce qui n'est par essence qu'"// &
                                   "un moyen, et te trompant ainsi sur la route "//A_GRAVE//" suivre les voil"//A_GRAVE// &
                                   " bi"//"ent"//O_CIRCUMFLEX//"t qui te d"//E_ACUTE// &
                                   "gradent, car si leur musique est vulgaire ils te f"// &
                                   "abriquent pour te la vendre une "//A_CIRCUMFLEX//"me vulgaire."

contains

    !> Collect all exported unit tests.
    subroutine collect_quoted_printable_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("qp-test--quoted-printable-decode-region", test_decode_region), &
                    new_unittest("qp-test--quoted-printable-decode-string", test_decode_string), &
                    new_unittest("qp-test--quoted-printable-encode-region", test_encode_region), &
                    new_unittest("qp-test--quoted-printable-encode-string", test_encode_string) &
                    ]
    end subroutine collect_quoted_printable_mod

    !> (with-temp-buffer (insert TEXT)).
    subroutine temporary_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *qp-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temporary_buffer

    subroutine check_string(error, got, wanted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: got, wanted

        call check(error, got == wanted, "got """//got//""", wanted """//wanted//"""")
    end subroutine check_string

    subroutine test_decode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: inserted

        call temporary_buffer(QUOTE_QP, h)
        call encode_coding_region(point_minimum(), point_maximum(), "utf-8", inserted, st)
        if (st%ok()) call decode_quoted_printable_region(point_minimum(), point_maximum(), st, "utf-8")
        call check_string(error, buffer_string(), QUOTE_UTF8)
        call close_buffer(h, st)
    end subroutine test_decode_region

    subroutine test_decode_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, decode_quoted_printable_string("foo!"), "foo!")
        if (allocated(error)) return
        call check_string(error, decode_quoted_printable_string("=0C"), achar(12))
        if (allocated(error)) return
        call check_string(error, decode_quoted_printable_string("=3D"), "=")
        if (allocated(error)) return
        call check_string(error, decode_quoted_printable_string("=A1Hola, se=F1or!?"), char(161)//"Hola, se"//char(241)//"or!?")
    end subroutine test_decode_string

    subroutine test_encode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(repeat("=", 26), h)
        call encode_quoted_printable_region(point_minimum(), point_maximum(), st, .true.)
        call check_string(error, buffer_string(), "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D"// &
                          "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D="//LF//"=3D")
        call close_buffer(h, st)
    end subroutine test_encode_region

    subroutine test_encode_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: encoded

        call check_string(error, encode_quoted_printable_string(char(161)//"Hola, se"//char(241)//"or!?"), &
                          "=A1Hola, se=F1or!?")
        if (allocated(error)) return
        ! Multibyte character.
        encoded = encode_quoted_printable_string(char(195)//char(165), .true., st)
        call check(error, format_error(st) == "Multibyte character in QP encoding region", format_error(st))
    end subroutine test_encode_string
end module test_quoted_printable_mod
