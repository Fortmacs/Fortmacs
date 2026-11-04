! test_sha1_mod.f90 -- tests for sha1_mod (the SHA-1 message digest).
! Own tests; GNU Emacs 31.1 has no tests of its Gnulib digests: the
! known answers of FIPS 180 and digests fed in pieces.
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

!> Tests for sha1_mod: the known answers of FIPS 180 (the empty message,
!> "abc", the 448-bit and 896-bit messages, a million "a"s) and the
!> same digests from the bytes fed in pieces of every size.
module test_sha1_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use sha1_mod, only: compute_sha1, sha1_context_type
    implicit none(type, external)
    private

    public :: collect_sha1_mod

    !> The 448-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_448 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    !> The 896-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_896 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"// &
                                   "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

contains

    !> Collect all exported unit tests.
    subroutine collect_sha1_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("sha1-known-answers", test_sha1_known_answers), &
                    new_unittest("sha1-pieces", test_sha1_pieces) &
                    ]
    end subroutine collect_sha1_mod

    !> The known answers of SHA1.
    subroutine test_sha1_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha1("")) == &
                   "da39a3ee5e6b4b0d3255bfef95601890afd80709", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha1("abc")) == &
                   "a9993e364706816aba3e25717850c26c9cd0d89d", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha1(MESSAGE_448)) == &
                   "84983e441c3bd26ebaae4aa1f95129e5e54670f1", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha1(MESSAGE_896)) == &
                   "a49b2446a02c645bf419f995b67091253a04a259", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha1(repeat("a", 1000000))) == &
                   "34aa973cd4c4daa4f61eeb2bdbad27316534016f", "a million a characters")
    end subroutine test_sha1_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha1_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha1_context_type) :: context
        integer :: piece, i

        whole = compute_sha1(data)

        do piece = 1, len(data)
            context = sha1_context_type()
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha1_pieces

    !> The lowercase hexadecimal digits of the bytes of DIGEST.
    pure function hex(digest) result(text)
        character(len=*), intent(in) :: digest
        character(len=2*len(digest)) :: text
        character(len=16), parameter :: HEX_DIGITS = "0123456789abcdef"
        integer :: i, byte

        do i = 1, len(digest)
            byte = iachar(digest(i:i))
            text(2*i - 1:2*i - 1) = HEX_DIGITS(byte/16 + 1:byte/16 + 1)
            text(2*i:2*i) = HEX_DIGITS(mod(byte, 16) + 1:mod(byte, 16) + 1)
        end do
    end function hex
end module test_sha1_mod
