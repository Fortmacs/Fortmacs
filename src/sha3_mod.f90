! sha3_mod.f90 -- the SHA-3 message digests.
! Ported from GNU Emacs 31.1 lib/sha3.c.
!
! Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

!> The SHA3-224, SHA3-256, SHA3-384 and SHA3-512 message digests of
!> FIPS 202 (Gnulib's sha3.c, which fns.c's `secure_hash' uses): the
!> Keccak-f[1600] permutation over 25 64-bit lanes, held as int64 bit
!> patterns.  A digest is a string of SHA3_224_DIGEST_SIZE to
!> SHA3_512_DIGEST_SIZE bytes.
module sha3_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none(type, external)
    private

    !> The size of a SHA3-224 digest in bytes (sha3.h:SHA3_224_DIGEST_SIZE).
    integer, parameter, public :: SHA3_224_DIGEST_SIZE = 28
    !> The size of a SHA3-256 digest in bytes (sha3.h:SHA3_256_DIGEST_SIZE).
    integer, parameter, public :: SHA3_256_DIGEST_SIZE = 32
    !> The size of a SHA3-384 digest in bytes (sha3.h:SHA3_384_DIGEST_SIZE).
    integer, parameter, public :: SHA3_384_DIGEST_SIZE = 48
    !> The size of a SHA3-512 digest in bytes (sha3.h:SHA3_512_DIGEST_SIZE).
    integer, parameter, public :: SHA3_512_DIGEST_SIZE = 64

    !> The size of the state, in bytes; a digest of N bytes absorbs blocks
    !> of STATE_BYTES - 2*N bytes (sha3.h:SHA3_224_BLOCK_SIZE and the others).
    integer, parameter :: STATE_BYTES = 200
    !> The largest block, SHA3-224's.
    integer, parameter :: BLOCK_SIZE_MAX = STATE_BYTES - 2*SHA3_224_DIGEST_SIZE
    !> The round constants of the Iota step (sha3.c:rc).
    integer(int64), parameter :: ROUND_CONSTANTS(0:23) = [int(z'0000000000000001', int64), int(z'0000000000008082', int64), &
                                                          int(z'800000000000808A', int64), int(z'8000000080008000', int64), &
                                                          int(z'000000000000808B', int64), int(z'0000000080000001', int64), &
                                                          int(z'8000000080008081', int64), int(z'8000000000008009', int64), &
                                                          int(z'000000000000008A', int64), int(z'0000000000000088', int64), &
                                                          int(z'0000000080008009', int64), int(z'000000008000000A', int64), &
                                                          int(z'000000008000808B', int64), int(z'800000000000008B', int64), &
                                                          int(z'8000000000008089', int64), int(z'8000000000008003', int64), &
                                                          int(z'8000000000008002', int64), int(z'8000000000000080', int64), &
                                                          int(z'000000000000800A', int64), int(z'800000008000000A', int64), &
                                                          int(z'8000000080008081', int64), int(z'8000000000008080', int64), &
                                                          int(z'0000000080000001', int64), int(z'8000000080008008', int64)]
    !> The lanes the Rho and Pi steps visit in turn, after lane 1.
    integer, parameter :: PI_LANES(24) = [10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19, 13, 12, 2, 20, 14, &
                                          22, 9, 6, 1]
    !> The rotation of each lane the Rho and Pi steps move.
    integer, parameter :: RHO_SHIFTS(24) = [1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14, 27, 41, 56, 8, 25, 43, 62, &
                                            18, 39, 61, 20, 44]

    !> The state of a SHA-3 computation, made with the size of its digest:
    !> `sha3_context_type(digest_size=SHA3_256_DIGEST_SIZE)' (sha3.c's
    !> sha3_224_init_ctx to sha3_512_init_ctx).  Feed it with `call
    !> context%process_bytes(data)' and get the digest with `call
    !> context%finish(digest)'.  Port of sha3.h:struct sha3_ctx.
    type, public :: sha3_context_type
        !> The size of the digest, in bytes; one of SHA3_224_DIGEST_SIZE to
        !> SHA3_512_DIGEST_SIZE (upstream's digestlen).
        integer :: digest_size
        !> The 25 lanes of the Keccak state.
        integer(int64) :: state(0:24) = 0
        !> The bytes of an incomplete block, waiting for more.
        character(len=BLOCK_SIZE_MAX) :: buffer = ""
        !> The number of bytes in `buffer' (upstream's buflen).
        integer :: buffer_length = 0
    contains
        procedure :: process_bytes => process_sha3_bytes
        procedure :: finish => finish_sha3
    end type sha3_context_type

    public :: compute_sha3

contains

    !> The SHA-3 digest of DIGEST_SIZE bytes (SHA3_224_DIGEST_SIZE to
    !> SHA3_512_DIGEST_SIZE) of the bytes of DATA.  Port of
    !> sha3.c:sha3_224_buffer, sha3_256_buffer, sha3_384_buffer and
    !> sha3_512_buffer.
    pure function compute_sha3(data, digest_size) result(digest)
        character(len=*), intent(in) :: data
        integer, intent(in) :: digest_size
        character(len=:), allocatable :: digest
        type(sha3_context_type) :: context

        context = sha3_context_type(digest_size=digest_size)
        call context%process_bytes(data)
        call context%finish(digest)
    end function compute_sha3

    !> Add the bytes of DATA to the computation SELF: whole blocks are
    !> absorbed at once, the rest kept for the next call or `finish'.
    !> Port of sha3.c:sha3_process_bytes.
    pure subroutine process_sha3_bytes(self, data)
        class(sha3_context_type), intent(inout) :: self
        character(len=*), intent(in) :: data
        integer(int64) :: i, n, add
        integer :: block_size

        block_size = STATE_BYTES - 2*self%digest_size
        n = len(data, kind=int64)
        i = 1

        if (self%buffer_length > 0) then
            add = min(int(block_size - self%buffer_length, int64), n)
            self%buffer(self%buffer_length + 1:self%buffer_length + add) = data(1:add)
            self%buffer_length = self%buffer_length + int(add)
            i = add + 1
            ! Not enough to fill a full block.
            if (self%buffer_length < block_size) return
            ! Process the block that already had bytes buffered.
            call process_sha3_block(self%state, self%buffer(1:block_size))
            self%buffer_length = 0
        end if

        ! Process as many complete blocks as possible.
        do while (n - i + 1 >= block_size)
            call process_sha3_block(self%state, data(i:i + block_size - 1))
            i = i + block_size
        end do

        if (i <= n) then
            self%buffer(1:n - i + 1) = data(i:n)
            self%buffer_length = int(n - i + 1)
        end if
    end subroutine process_sha3_bytes

    !> Pad the remaining bytes of SELF as FIPS 202 says for SHA-3 (the
    !> suffix 01, then 10*1), absorb them and give the DIGEST, the first
    !> `digest_size' bytes of the state in little-endian byte order.
    !> Port of sha3.c:sha3_finish_ctx (with sha3.c:sha3_conclude_ctx and
    !> sha3_read_ctx).
    pure subroutine finish_sha3(self, digest)
        class(sha3_context_type), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: digest
        character(len=BLOCK_SIZE_MAX) :: last
        integer :: block_size, k

        block_size = STATE_BYTES - 2*self%digest_size
        last = repeat(achar(0), BLOCK_SIZE_MAX)
        last(1:self%buffer_length) = self%buffer(1:self%buffer_length)
        last(self%buffer_length + 1:self%buffer_length + 1) = achar(6)
        last(block_size:block_size) = char(ior(iachar(last(block_size:block_size)), 128))
        call process_sha3_block(self%state, last(1:block_size))
        self%buffer_length = 0
        allocate (character(len=self%digest_size) :: digest)

        do k = 0, self%digest_size - 1
            digest(k + 1:k + 1) = char(ibits(self%state(k/8), 8*mod(k, 8), 8))
        end do
    end subroutine finish_sha3

    !> XOR the little-endian 64-bit words of BLOCK (a whole block of the
    !> digest's size) into the first lanes of STATE and apply the 24
    !> rounds of Keccak-f[1600] (Theta, Rho and Pi, Chi, Iota).  Port of
    !> sha3.c:sha3_process_block (one block per call; Rho and Pi from
    !> tables).
    pure subroutine process_sha3_block(state, block)
        integer(int64), intent(inout) :: state(0:24)
        character(len=*), intent(in) :: block
        integer(int64) :: word, c(0:4), d(0:4), t1, t2
        integer :: i, j, k

        do i = 0, len(block)/8 - 1
            word = 0

            do k = 7, 0, -1
                word = ior(ishft(word, 8), int(iachar(block(8*i + k + 1:8*i + k + 1)), int64))
            end do

            state(i) = ieor(state(i), word)
        end do

        do i = 0, 23
            ! Theta.
            do j = 0, 4
                c(j) = ieor(ieor(ieor(ieor(state(j), state(j + 5)), state(j + 10)), state(j + 15)), state(j + 20))
            end do

            do j = 0, 4
                d(j) = ieor(c(mod(j + 4, 5)), ishftc(c(mod(j + 1, 5)), 1))
            end do

            do j = 0, 24
                state(j) = ieor(state(j), d(mod(j, 5)))
            end do

            ! Rho and Pi.
            t1 = state(1)

            do k = 1, 24
                t2 = ishftc(t1, RHO_SHIFTS(k))
                t1 = state(PI_LANES(k))
                state(PI_LANES(k)) = t2
            end do

            ! Chi.
            do j = 0, 20, 5
                t1 = state(j)
                t2 = state(j + 1)
                state(j) = ieor(state(j), iand(not(state(j + 1)), state(j + 2)))
                state(j + 1) = ieor(state(j + 1), iand(not(state(j + 2)), state(j + 3)))
                state(j + 2) = ieor(state(j + 2), iand(not(state(j + 3)), state(j + 4)))
                state(j + 3) = ieor(state(j + 3), iand(not(state(j + 4)), t1))
                state(j + 4) = ieor(state(j + 4), iand(not(t1), t2))
            end do

            ! Iota.
            state(0) = ieor(state(0), ROUND_CONSTANTS(i))
        end do
    end subroutine process_sha3_block
end module sha3_mod
