! test_sha512_mod.f90 -- tests for sha512_mod (the SHA-384 and SHA-512 message digests).
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

!> Tests for sha512_mod: the known answers of FIPS 180 (the empty message,
!> "abc", the 448-bit and 896-bit messages, a million "a"s) and the
!> same digests from the bytes fed in pieces of every size.
module test_sha512_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use sha512_mod, only: compute_sha384, compute_sha512, sha512_context_type, SHA384_INITIAL_STATE, SHA384_DIGEST_SIZE
    implicit none(type, external)
    private

    public :: collect_sha512_mod

    !> The 448-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_448 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    !> The 896-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_896 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"// &
                                   "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

contains

    !> Collect all exported unit tests.
    subroutine collect_sha512_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("sha384-known-answers", test_sha384_known_answers), &
                    new_unittest("sha384-pieces", test_sha384_pieces), &
                    new_unittest("sha512-known-answers", test_sha512_known_answers), &
                    new_unittest("sha512-pieces", test_sha512_pieces) &
                    ]
    end subroutine collect_sha512_mod

    !> The known answers of SHA384.
    subroutine test_sha384_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha384("")) == &
                   "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da"// &
                   "274edebfe76f65fbd51ad2f14898b95b", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha384("abc")) == &
                   "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed"// &
                   "8086072ba1e7cc2358baeca134c825a7", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha384(MESSAGE_448)) == &
                   "3391fdddfc8dc7393707a65b1b4709397cf8b1d162af05abfe8f450de5f36bc6"// &
                   "b0455a8520bc4e6f5fe95b1fe3c8452b", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha384(MESSAGE_896)) == &
                   "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712"// &
                   "fcc7c71a557e2db966c3e9fa91746039", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha384(repeat("a", 1000000))) == &
                   "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b"// &
                   "07b8b3dc38ecc4ebae97ddd87f3d8985", "a million a characters")
    end subroutine test_sha384_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha384_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha512_context_type) :: context
        integer :: piece, i

        whole = compute_sha384(data)

        do piece = 1, len(data)
            context = sha512_context_type(state=SHA384_INITIAL_STATE, digest_size=SHA384_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha384_pieces

    !> The known answers of SHA512.
    subroutine test_sha512_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha512("")) == &
                   "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce"// &
                   "47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha512("abc")) == &
                   "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a"// &
                   "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha512(MESSAGE_448)) == &
                   "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c335"// &
                   "96fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha512(MESSAGE_896)) == &
                   "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018"// &
                   "501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha512(repeat("a", 1000000))) == &
                   "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973eb"// &
                   "de0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b", "a million a characters")
    end subroutine test_sha512_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha512_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha512_context_type) :: context
        integer :: piece, i

        whole = compute_sha512(data)

        do piece = 1, len(data)
            context = sha512_context_type()
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha512_pieces

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
end module test_sha512_mod
