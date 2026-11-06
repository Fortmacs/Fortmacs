! test_sha256_mod.f90 -- tests for sha256_mod (the SHA-224 and SHA-256 message digests).
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

!> Tests for sha256_mod: the known answers of FIPS 180 (the empty message,
!> "abc", the 448-bit and 896-bit messages, a million "a"s) and the
!> same digests from the bytes fed in pieces of every size.
module test_sha256_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use sha256_mod, only: compute_sha224, compute_sha256, sha256_context_type, SHA224_INITIAL_STATE, SHA224_DIGEST_SIZE
    implicit none(type, external)
    private

    public :: collect_sha256_mod

    !> The 448-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_448 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    !> The 896-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_896 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"// &
                                   "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

contains

    !> Collect all exported unit tests.
    subroutine collect_sha256_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("sha224-known-answers", test_sha224_known_answers), &
                    new_unittest("sha224-pieces", test_sha224_pieces), &
                    new_unittest("sha256-known-answers", test_sha256_known_answers), &
                    new_unittest("sha256-pieces", test_sha256_pieces) &
                    ]
    end subroutine collect_sha256_mod

    !> The known answers of SHA224.
    subroutine test_sha224_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hexadecimal(compute_sha224("")) == &
                   "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f", "empty")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha224("abc")) == &
                   "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7", "abc")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha224(MESSAGE_448)) == &
                   "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha224(MESSAGE_896)) == &
                   "c97ca9a559850ce97a04a96def6d99a9e0e0e2ab14e6b8df265fc0b3", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha224(repeat("a", 1000000))) == &
                   "20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67", "a million a characters")
    end subroutine test_sha224_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha224_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha256_context_type) :: context
        integer :: piece, i

        whole = compute_sha224(data)

        do piece = 1, len(data)
            context = sha256_context_type(state=SHA224_INITIAL_STATE, digest_size=SHA224_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha224_pieces

    !> The known answers of SHA256.
    subroutine test_sha256_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hexadecimal(compute_sha256("")) == &
                   "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "empty")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha256("abc")) == &
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", "abc")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha256(MESSAGE_448)) == &
                   "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha256(MESSAGE_896)) == &
                   "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hexadecimal(compute_sha256(repeat("a", 1000000))) == &
                   "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0", "a million a characters")
    end subroutine test_sha256_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha256_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha256_context_type) :: context
        integer :: piece, i

        whole = compute_sha256(data)

        do piece = 1, len(data)
            context = sha256_context_type()
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha256_pieces

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
end module test_sha256_mod
