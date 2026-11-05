! test_md5_mod.f90 -- tests for md5_mod (the MD5 message digest).
! Own tests; GNU Emacs 31.1 has no tests of its Gnulib digests: the
! known answers of RFC 1321 and digests fed in pieces.
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

!> Tests for md5_mod: the known answers of RFC 1321 (the empty message,
!> "abc", the 448-bit and 896-bit messages, a million "a"s) and the
!> same digests from the bytes fed in pieces of every size.
module test_md5_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use md5_mod, only: compute_md5, md5_context_type
    implicit none(type, external)
    private

    public :: collect_md5_mod

    !> The 448-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_448 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    !> The 896-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_896 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"// &
                                   "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

contains

    !> Collect all exported unit tests.
    subroutine collect_md5_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("md5-known-answers", test_md5_known_answers), &
                    new_unittest("md5-pieces", test_md5_pieces) &
                    ]
    end subroutine collect_md5_mod

    !> The known answers of MD5.
    subroutine test_md5_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hexadecimal(compute_md5("")) == &
                   "d41d8cd98f00b204e9800998ecf8427e", "empty")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_md5("abc")) == &
                   "900150983cd24fb0d6963f7d28e17f72", "abc")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_md5(MESSAGE_448)) == &
                   "8215ef0796a20bcaaae116d3876c664a", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_md5(MESSAGE_896)) == &
                   "03dd8807a93175fb062dfb55dc7d359c", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_md5(repeat("a", 1000000))) == &
                   "7707d6ae4e027c70eea2a935c2296f21", "a million a characters")
    end subroutine test_md5_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_md5_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(md5_context_type) :: context
        integer :: piece, i

        whole = compute_md5(data)

        do piece = 1, len(data)
            context = md5_context_type()
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_md5_pieces

    !> The lowercase hexadecimal digits of the bytes of DIGEST.
    pure function hexadecimal(digest) result(text)
        character(len=*), intent(in) :: digest
        character(len=2*len(digest)) :: text
        character(len=16), parameter :: HEXADECIMAL_DIGITS = "0123456789abcdef"
        integer :: i, byte

        do i = 1, len(digest)
            byte = iachar(digest(i:i))
            text(2*i - 1:2*i - 1) = HEXADECIMAL_DIGITS(byte/16 + 1:byte/16 + 1)
            text(2*i:2*i) = HEXADECIMAL_DIGITS(mod(byte, 16) + 1:mod(byte, 16) + 1)
        end do
    end function hexadecimal
end module test_md5_mod
