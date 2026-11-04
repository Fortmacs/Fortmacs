! test_sha3_mod.f90 -- tests for sha3_mod (the SHA-3 message digests).
! Own tests; GNU Emacs 31.1 has no tests of its Gnulib digests: the
! known answers of FIPS 202 and digests fed in pieces.
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

!> Tests for sha3_mod: the known answers of FIPS 202 (the empty message,
!> "abc", the 448-bit and 896-bit messages, a million "a"s) and the
!> same digests from the bytes fed in pieces of every size.
module test_sha3_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use sha3_mod, only: compute_sha3, sha3_context_type, SHA3_224_DIGEST_SIZE, SHA3_256_DIGEST_SIZE, &
                        SHA3_384_DIGEST_SIZE, SHA3_512_DIGEST_SIZE
    implicit none(type, external)
    private

    public :: collect_sha3_mod

    !> The 448-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_448 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    !> The 896-bit message of the FIPS 180 examples.
    character(len=*), parameter :: MESSAGE_896 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"// &
                                   "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

contains

    !> Collect all exported unit tests.
    subroutine collect_sha3_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("sha3-224-known-answers", test_sha3_224_known_answers), &
                    new_unittest("sha3-224-pieces", test_sha3_224_pieces), &
                    new_unittest("sha3-256-known-answers", test_sha3_256_known_answers), &
                    new_unittest("sha3-256-pieces", test_sha3_256_pieces), &
                    new_unittest("sha3-384-known-answers", test_sha3_384_known_answers), &
                    new_unittest("sha3-384-pieces", test_sha3_384_pieces), &
                    new_unittest("sha3-512-known-answers", test_sha3_512_known_answers), &
                    new_unittest("sha3-512-pieces", test_sha3_512_pieces) &
                    ]
    end subroutine collect_sha3_mod

    !> The known answers of SHA3-224.
    subroutine test_sha3_224_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha3("", SHA3_224_DIGEST_SIZE)) == &
                   "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha3("abc", SHA3_224_DIGEST_SIZE)) == &
                   "e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_448, SHA3_224_DIGEST_SIZE)) == &
                   "8a24108b154ada21c9fd5574494479ba5c7e7ab76ef264ead0fcce33", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_896, SHA3_224_DIGEST_SIZE)) == &
                   "543e6868e1666c1a643630df77367ae5a62a85070a51c14cbf665cbc", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(repeat("a", 1000000), SHA3_224_DIGEST_SIZE)) == &
                   "d69335b93325192e516a912e6d19a15cb51c6ed5c15243e7a7fd653c", "a million a characters")
    end subroutine test_sha3_224_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha3_224_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha3_context_type) :: context
        integer :: piece, i

        whole = compute_sha3(data, SHA3_224_DIGEST_SIZE)

        do piece = 1, len(data)
            context = sha3_context_type(digest_size=SHA3_224_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha3_224_pieces

    !> The known answers of SHA3-256.
    subroutine test_sha3_256_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha3("", SHA3_256_DIGEST_SIZE)) == &
                   "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha3("abc", SHA3_256_DIGEST_SIZE)) == &
                   "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_448, SHA3_256_DIGEST_SIZE)) == &
                   "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_896, SHA3_256_DIGEST_SIZE)) == &
                   "916f6061fe879741ca6469b43971dfdb28b1a32dc36cb3254e812be27aad1d18", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(repeat("a", 1000000), SHA3_256_DIGEST_SIZE)) == &
                   "5c8875ae474a3634ba4fd55ec85bffd661f32aca75c6d699d0cdcb6c115891c1", "a million a characters")
    end subroutine test_sha3_256_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha3_256_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha3_context_type) :: context
        integer :: piece, i

        whole = compute_sha3(data, SHA3_256_DIGEST_SIZE)

        do piece = 1, len(data)
            context = sha3_context_type(digest_size=SHA3_256_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha3_256_pieces

    !> The known answers of SHA3-384.
    subroutine test_sha3_384_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha3("", SHA3_384_DIGEST_SIZE)) == &
                   "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2a"// &
                   "c3713831264adb47fb6bd1e058d5f004", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha3("abc", SHA3_384_DIGEST_SIZE)) == &
                   "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b2"// &
                   "98d88cea927ac7f539f1edf228376d25", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_448, SHA3_384_DIGEST_SIZE)) == &
                   "991c665755eb3a4b6bbdfb75c78a492e8c56a22c5c4d7e429bfdbc32b9d4ad5a"// &
                   "a04a1f076e62fea19eef51acd0657c22", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_896, SHA3_384_DIGEST_SIZE)) == &
                   "79407d3b5916b59c3e30b09822974791c313fb9ecc849e406f23592d04f625dc"// &
                   "8c709b98b43b3852b337216179aa7fc7", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(repeat("a", 1000000), SHA3_384_DIGEST_SIZE)) == &
                   "eee9e24d78c1855337983451df97c8ad9eedf256c6334f8e948d252d5e0e7684"// &
                   "7aa0774ddb90a842190d2c558b4b8340", "a million a characters")
    end subroutine test_sha3_384_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha3_384_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha3_context_type) :: context
        integer :: piece, i

        whole = compute_sha3(data, SHA3_384_DIGEST_SIZE)

        do piece = 1, len(data)
            context = sha3_context_type(digest_size=SHA3_384_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha3_384_pieces

    !> The known answers of SHA3-512.
    subroutine test_sha3_512_known_answers(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, hex(compute_sha3("", SHA3_512_DIGEST_SIZE)) == &
                   "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6"// &
                   "15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26", "empty")
        if (allocated(error)) return
        call check(error, hex(compute_sha3("abc", SHA3_512_DIGEST_SIZE)) == &
                   "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e"// &
                   "10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0", "abc")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_448, SHA3_512_DIGEST_SIZE)) == &
                   "04a371e84ecfb5b8b77cb48610fca8182dd457ce6f326a0fd3d7ec2f1e91636d"// &
                   "ee691fbe0c985302ba1b0d8dc78c086346b533b49c030d99a27daf1139d6e75e", "the 448-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(MESSAGE_896, SHA3_512_DIGEST_SIZE)) == &
                   "afebb2ef542e6579c50cad06d2e578f9f8dd6881d7dc824d26360feebf18a4fa"// &
                   "73e3261122948efcfd492e74e82e2189ed0fb440d187f382270cb455f21dd185", "the 896-bit message")
        if (allocated(error)) return
        call check(error, hex(compute_sha3(repeat("a", 1000000), SHA3_512_DIGEST_SIZE)) == &
                   "3c3a876da14034ab60627c077bb98f7e120a2a5370212dffb3385a18d4f38859"// &
                   "ed311d0a9d5141ce9cc5c66ee689b266a8aa18ace8282a0e0db596c90b0a7b87", "a million a characters")
    end subroutine test_sha3_512_known_answers

    !> The digest of 300 bytes fed in pieces of 1 to 300 bytes is the
    !> digest of the whole.
    subroutine test_sha3_512_pieces(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: data = MESSAGE_896//repeat("x", 188)
        character(len=:), allocatable :: whole, digest
        type(sha3_context_type) :: context
        integer :: piece, i

        whole = compute_sha3(data, SHA3_512_DIGEST_SIZE)

        do piece = 1, len(data)
            context = sha3_context_type(digest_size=SHA3_512_DIGEST_SIZE)
            i = 1

            do while (i <= len(data))
                call context%process_bytes(data(i:min(i + piece - 1, len(data))))
                i = i + piece
            end do

            call context%finish(digest)
            call check(error, digest == whole, "pieces")
            if (allocated(error)) return
        end do
    end subroutine test_sha3_512_pieces

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
end module test_sha3_mod
