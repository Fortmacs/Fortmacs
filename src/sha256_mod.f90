! sha256_mod.f90 -- the SHA-224 and SHA-256 message digests.
! Ported from GNU Emacs 31.1 lib/sha256.c.
!
! Copyright (C) 2005-2006, 2008-2026 Free Software Foundation, Inc.
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

!> The SHA-224 and SHA-256 message digests of FIPS 180-2 (Gnulib's
!> sha256.c, which fns.c's `secure_hash' uses).  A digest is a string of
!> SHA224_DIGEST_SIZE or SHA256_DIGEST_SIZE bytes; the 32-bit words of
!> the algorithm are kept in int64 variables masked to their low 32
!> bits, Fortran having no unsigned type.
module sha256_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none(type, external)
    private

    !> The size of a SHA-224 digest in bytes (sha256.h:SHA224_DIGEST_SIZE).
    integer, parameter, public :: SHA224_DIGEST_SIZE = 28
    !> The size of a SHA-256 digest in bytes (sha256.h:SHA256_DIGEST_SIZE).
    integer, parameter, public :: SHA256_DIGEST_SIZE = 32
    !> The size of the blocks SHA-224 and SHA-256 process, in bytes.
    integer, parameter, public :: SHA256_BLOCK_SIZE = 64

    !> The initial values of SHA-224 (sha256.c:sha224_init_ctx), for
    !> `sha256_context_type(state=SHA224_INITIAL_STATE,
    !> digest_size=SHA224_DIGEST_SIZE)'.
    integer(int64), parameter, public :: SHA224_INITIAL_STATE(8) = [int(z'c1059ed8', int64), int(z'367cd507', int64), &
                                                                    int(z'3070dd17', int64), int(z'f70e5939', int64), &
                                                                    int(z'ffc00b31', int64), int(z'68581511', int64), &
                                                                    int(z'64f98fa7', int64), int(z'befa4fa4', int64)]

    !> The low 32 bits of an int64.
    integer(int64), parameter :: MASK32 = int(z'ffffffff', int64)
    !> The constant of each of the 64 steps (sha256.c:sha256_round_constants).
    integer(int64), parameter :: ROUND_CONSTANTS(64) = [int(z'428a2f98', int64), int(z'71374491', int64), &
                                                        int(z'b5c0fbcf', int64), int(z'e9b5dba5', int64), &
                                                        int(z'3956c25b', int64), int(z'59f111f1', int64), &
                                                        int(z'923f82a4', int64), int(z'ab1c5ed5', int64), &
                                                        int(z'd807aa98', int64), int(z'12835b01', int64), &
                                                        int(z'243185be', int64), int(z'550c7dc3', int64), &
                                                        int(z'72be5d74', int64), int(z'80deb1fe', int64), &
                                                        int(z'9bdc06a7', int64), int(z'c19bf174', int64), &
                                                        int(z'e49b69c1', int64), int(z'efbe4786', int64), &
                                                        int(z'0fc19dc6', int64), int(z'240ca1cc', int64), &
                                                        int(z'2de92c6f', int64), int(z'4a7484aa', int64), &
                                                        int(z'5cb0a9dc', int64), int(z'76f988da', int64), &
                                                        int(z'983e5152', int64), int(z'a831c66d', int64), &
                                                        int(z'b00327c8', int64), int(z'bf597fc7', int64), &
                                                        int(z'c6e00bf3', int64), int(z'd5a79147', int64), &
                                                        int(z'06ca6351', int64), int(z'14292967', int64), &
                                                        int(z'27b70a85', int64), int(z'2e1b2138', int64), &
                                                        int(z'4d2c6dfc', int64), int(z'53380d13', int64), &
                                                        int(z'650a7354', int64), int(z'766a0abb', int64), &
                                                        int(z'81c2c92e', int64), int(z'92722c85', int64), &
                                                        int(z'a2bfe8a1', int64), int(z'a81a664b', int64), &
                                                        int(z'c24b8b70', int64), int(z'c76c51a3', int64), &
                                                        int(z'd192e819', int64), int(z'd6990624', int64), &
                                                        int(z'f40e3585', int64), int(z'106aa070', int64), &
                                                        int(z'19a4c116', int64), int(z'1e376c08', int64), &
                                                        int(z'2748774c', int64), int(z'34b0bcb5', int64), &
                                                        int(z'391c0cb3', int64), int(z'4ed8aa4a', int64), &
                                                        int(z'5b9cca4f', int64), int(z'682e6ff3', int64), &
                                                        int(z'748f82ee', int64), int(z'78a5636f', int64), &
                                                        int(z'84c87814', int64), int(z'8cc70208', int64), &
                                                        int(z'90befffa', int64), int(z'a4506ceb', int64), &
                                                        int(z'bef9a3f7', int64), int(z'c67178f2', int64)]

    !> The state of a SHA-256 computation, or of a SHA-224 one when made
    !> as `sha256_context_type(state=SHA224_INITIAL_STATE,
    !> digest_size=SHA224_DIGEST_SIZE)'.  A new object starts a SHA-256
    !> computation, with the initial values of FIPS 180-2
    !> (sha256.c:sha256_init_ctx).  Feed it with `call
    !> context%process_bytes(data)' and get the digest with `call
    !> context%finish(digest)'.  Port of sha256.h:struct sha256_ctx.
    type, public :: sha256_context_type
        !> The eight chaining values.
        integer(int64) :: state(8) = [int(z'6a09e667', int64), int(z'bb67ae85', int64), int(z'3c6ef372', int64), &
                                      int(z'a54ff53a', int64), int(z'510e527f', int64), int(z'9b05688c', int64), &
                                      int(z'1f83d9ab', int64), int(z'5be0cd19', int64)]
        !> The size of the digest `finish' gives, in bytes.
        integer :: digest_size = SHA256_DIGEST_SIZE
        !> The number of bytes processed so far (upstream's total).
        integer(int64) :: total = 0
        !> The bytes of an incomplete block, waiting for more.
        character(len=SHA256_BLOCK_SIZE) :: buffer = ""
        !> The number of bytes in `buffer' (upstream's buflen).
        integer :: buffer_length = 0
    contains
        procedure :: process_bytes => process_sha256_bytes
        procedure :: finish => finish_sha256
    end type sha256_context_type

    public :: compute_sha224, compute_sha256

contains

    !> The SHA-224 digest of the bytes of DATA.  Port of
    !> sha256.c:sha224_buffer.
    pure function compute_sha224(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=SHA224_DIGEST_SIZE) :: digest
        type(sha256_context_type) :: context
        character(len=:), allocatable :: full

        context = sha256_context_type(state=SHA224_INITIAL_STATE, digest_size=SHA224_DIGEST_SIZE)
        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_sha224

    !> The SHA-256 digest of the bytes of DATA.  Port of
    !> sha256.c:sha256_buffer.
    pure function compute_sha256(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=SHA256_DIGEST_SIZE) :: digest
        type(sha256_context_type) :: context
        character(len=:), allocatable :: full

        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_sha256

    !> Add the bytes of DATA to the computation SELF: whole blocks are
    !> processed at once, the rest kept for the next call or `finish'.
    !> Port of sha256.c:sha256_process_bytes (any alignment).
    pure subroutine process_sha256_bytes(self, data)
        class(sha256_context_type), intent(inout) :: self
        character(len=*), intent(in) :: data
        integer(int64) :: i, n, add

        n = len(data, kind=int64)
        self%total = self%total + n
        i = 1

        ! When we already have some bytes in our internal buffer,
        ! concatenate both inputs first.
        if (self%buffer_length > 0) then
            add = min(int(SHA256_BLOCK_SIZE - self%buffer_length, int64), n)
            self%buffer(self%buffer_length + 1:self%buffer_length + add) = data(1:add)
            self%buffer_length = self%buffer_length + int(add)
            i = add + 1
            if (self%buffer_length < SHA256_BLOCK_SIZE) return
            call process_sha256_block(self%state, self%buffer)
            self%buffer_length = 0
        end if

        ! Process available complete blocks.
        do while (n - i + 1 >= SHA256_BLOCK_SIZE)
            call process_sha256_block(self%state, data(i:i + SHA256_BLOCK_SIZE - 1))
            i = i + SHA256_BLOCK_SIZE
        end do

        ! Move remaining bytes into the internal buffer.
        if (i <= n) then
            self%buffer(1:n - i + 1) = data(i:n)
            self%buffer_length = int(n - i + 1)
        end if
    end subroutine process_sha256_bytes

    !> Process the remaining bytes of SELF, padded as FIPS 180-2 says (a 1
    !> bit, zeros, the length in bits as 64 bits) and give the DIGEST, the
    !> first `digest_size' bytes of the chaining values in big-endian byte
    !> order.  Port of sha256.c:sha256_finish_ctx and sha224_finish_ctx
    !> (with sha256.c:sha256_conclude_ctx, sha256_read_ctx and
    !> sha224_read_ctx).
    pure subroutine finish_sha256(self, digest)
        class(sha256_context_type), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: digest
        character(len=2*SHA256_BLOCK_SIZE) :: last
        character(len=SHA256_DIGEST_SIZE) :: full
        integer(int64) :: bits
        integer :: padded, i, k

        ! Take yet unprocessed bytes into account.
        padded = merge(SHA256_BLOCK_SIZE, 2*SHA256_BLOCK_SIZE, self%buffer_length < SHA256_BLOCK_SIZE - 8)
        last = repeat(achar(0), 2*SHA256_BLOCK_SIZE)
        last(1:self%buffer_length) = self%buffer(1:self%buffer_length)
        last(self%buffer_length + 1:self%buffer_length + 1) = char(128)
        ! Put the 64-bit file length in *bits* at the end of the buffer.
        bits = ishft(self%total, 3)

        do k = 0, 7
            last(padded - k:padded - k) = char(ibits(bits, 8*k, 8))
        end do

        ! Process last bytes.
        call process_sha256_block(self%state, last(1:SHA256_BLOCK_SIZE))
        if (padded > SHA256_BLOCK_SIZE) call process_sha256_block(self%state, last(SHA256_BLOCK_SIZE + 1:padded))
        self%buffer_length = 0

        do i = 1, 8
            do k = 0, 3
                full(4*i - k:4*i - k) = char(ibits(self%state(i), 8*k, 8))
            end do
        end do

        digest = full(1:self%digest_size)
    end subroutine finish_sha256

    !> Process the 64 bytes of BLOCK into the chaining values STATE: 64
    !> steps over its 16 big-endian words, expanded (FIPS 180-2, 6.2.2).
    !> Port of sha256.c:sha256_process_block (one block per call; the
    !> steps as a loop).
    pure subroutine process_sha256_block(state, block)
        integer(int64), intent(inout) :: state(8)
        character(len=SHA256_BLOCK_SIZE), intent(in) :: block
        integer(int64) :: x(0:63), v(8), t0, t1, s0, s1
        integer :: i, k

        do i = 0, 15
            x(i) = 0

            do k = 0, 3
                x(i) = ior(ishft(x(i), 8), int(iachar(block(4*i + k + 1:4*i + k + 1)), int64))
            end do
        end do

        do i = 16, 63
            ! S0 and S1 of sha256.c.
            s0 = ieor(ieor(ishftc(x(i - 15), 25, 32), ishftc(x(i - 15), 14, 32)), ishft(x(i - 15), -3))
            s1 = ieor(ieor(ishftc(x(i - 2), 15, 32), ishftc(x(i - 2), 13, 32)), ishft(x(i - 2), -10))
            x(i) = iand(s1 + x(i - 7) + s0 + x(i - 16), MASK32)
        end do

        v = state

        do i = 0, 63
            ! SS0(A) + F2(A, B, C).
            t0 = ieor(ieor(ishftc(v(1), 30, 32), ishftc(v(1), 19, 32)), ishftc(v(1), 10, 32)) &
                 + ior(iand(v(1), v(2)), iand(v(3), ior(v(1), v(2))))
            ! H + SS1(E) + F1(E, F, G) + K + M.
            t1 = v(8) + ieor(ieor(ishftc(v(5), 26, 32), ishftc(v(5), 21, 32)), ishftc(v(5), 7, 32)) &
                 + ieor(v(7), iand(v(5), ieor(v(6), v(7)))) + ROUND_CONSTANTS(i + 1) + x(i)
            v(2:8) = v(1:7)
            v(5) = iand(v(5) + t1, MASK32)
            v(1) = iand(t0 + t1, MASK32)
        end do

        state = iand(state + v, MASK32)
    end subroutine process_sha256_block
end module sha256_mod
