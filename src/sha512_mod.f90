! sha512_mod.f90 -- the SHA-384 and SHA-512 message digests.
! Ported from GNU Emacs 31.1 lib/sha512.c (and lib/u64.h's u64plus).
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

!> The SHA-384 and SHA-512 message digests of FIPS 180-2 (Gnulib's
!> sha512.c, which fns.c's `secure_hash' uses).  A digest is a string of
!> SHA384_DIGEST_SIZE or SHA512_DIGEST_SIZE bytes; the 64-bit words of
!> the algorithm are int64 bit patterns, added modulo 2**64 by
!> `add_unsigned', Fortran having no unsigned type.
module sha512_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none(type, external)
    private

    !> The size of a SHA-384 digest in bytes (sha512.h:SHA384_DIGEST_SIZE).
    integer, parameter, public :: SHA384_DIGEST_SIZE = 48
    !> The size of a SHA-512 digest in bytes (sha512.h:SHA512_DIGEST_SIZE).
    integer, parameter, public :: SHA512_DIGEST_SIZE = 64
    !> The size of the blocks SHA-384 and SHA-512 process, in bytes.
    integer, parameter, public :: SHA512_BLOCK_SIZE = 128

    !> The initial values of SHA-384 (sha512.c:sha384_init_ctx), for
    !> `sha512_context_type(state=SHA384_INITIAL_STATE,
    !> digest_size=SHA384_DIGEST_SIZE)'.
    integer(int64), parameter, public :: SHA384_INITIAL_STATE(8) = [int(z'cbbb9d5dc1059ed8', int64), &
                                                                    int(z'629a292a367cd507', int64), &
                                                                    int(z'9159015a3070dd17', int64), &
                                                                    int(z'152fecd8f70e5939', int64), &
                                                                    int(z'67332667ffc00b31', int64), &
                                                                    int(z'8eb44a8768581511', int64), &
                                                                    int(z'db0c2e0d64f98fa7', int64), &
                                                                    int(z'47b5481dbefa4fa4', int64)]

    !> The low 32 bits of an int64.
    integer(int64), parameter :: MASK32 = int(z'ffffffff', int64)
    !> The constant of each of the 80 steps (sha512.c:sha512_round_constants).
    integer(int64), parameter :: ROUND_CONSTANTS(80) = [int(z'428a2f98d728ae22', int64), int(z'7137449123ef65cd', int64), &
                                                        int(z'b5c0fbcfec4d3b2f', int64), int(z'e9b5dba58189dbbc', int64), &
                                                        int(z'3956c25bf348b538', int64), int(z'59f111f1b605d019', int64), &
                                                        int(z'923f82a4af194f9b', int64), int(z'ab1c5ed5da6d8118', int64), &
                                                        int(z'd807aa98a3030242', int64), int(z'12835b0145706fbe', int64), &
                                                        int(z'243185be4ee4b28c', int64), int(z'550c7dc3d5ffb4e2', int64), &
                                                        int(z'72be5d74f27b896f', int64), int(z'80deb1fe3b1696b1', int64), &
                                                        int(z'9bdc06a725c71235', int64), int(z'c19bf174cf692694', int64), &
                                                        int(z'e49b69c19ef14ad2', int64), int(z'efbe4786384f25e3', int64), &
                                                        int(z'0fc19dc68b8cd5b5', int64), int(z'240ca1cc77ac9c65', int64), &
                                                        int(z'2de92c6f592b0275', int64), int(z'4a7484aa6ea6e483', int64), &
                                                        int(z'5cb0a9dcbd41fbd4', int64), int(z'76f988da831153b5', int64), &
                                                        int(z'983e5152ee66dfab', int64), int(z'a831c66d2db43210', int64), &
                                                        int(z'b00327c898fb213f', int64), int(z'bf597fc7beef0ee4', int64), &
                                                        int(z'c6e00bf33da88fc2', int64), int(z'd5a79147930aa725', int64), &
                                                        int(z'06ca6351e003826f', int64), int(z'142929670a0e6e70', int64), &
                                                        int(z'27b70a8546d22ffc', int64), int(z'2e1b21385c26c926', int64), &
                                                        int(z'4d2c6dfc5ac42aed', int64), int(z'53380d139d95b3df', int64), &
                                                        int(z'650a73548baf63de', int64), int(z'766a0abb3c77b2a8', int64), &
                                                        int(z'81c2c92e47edaee6', int64), int(z'92722c851482353b', int64), &
                                                        int(z'a2bfe8a14cf10364', int64), int(z'a81a664bbc423001', int64), &
                                                        int(z'c24b8b70d0f89791', int64), int(z'c76c51a30654be30', int64), &
                                                        int(z'd192e819d6ef5218', int64), int(z'd69906245565a910', int64), &
                                                        int(z'f40e35855771202a', int64), int(z'106aa07032bbd1b8', int64), &
                                                        int(z'19a4c116b8d2d0c8', int64), int(z'1e376c085141ab53', int64), &
                                                        int(z'2748774cdf8eeb99', int64), int(z'34b0bcb5e19b48a8', int64), &
                                                        int(z'391c0cb3c5c95a63', int64), int(z'4ed8aa4ae3418acb', int64), &
                                                        int(z'5b9cca4f7763e373', int64), int(z'682e6ff3d6b2b8a3', int64), &
                                                        int(z'748f82ee5defb2fc', int64), int(z'78a5636f43172f60', int64), &
                                                        int(z'84c87814a1f0ab72', int64), int(z'8cc702081a6439ec', int64), &
                                                        int(z'90befffa23631e28', int64), int(z'a4506cebde82bde9', int64), &
                                                        int(z'bef9a3f7b2c67915', int64), int(z'c67178f2e372532b', int64), &
                                                        int(z'ca273eceea26619c', int64), int(z'd186b8c721c0c207', int64), &
                                                        int(z'eada7dd6cde0eb1e', int64), int(z'f57d4f7fee6ed178', int64), &
                                                        int(z'06f067aa72176fba', int64), int(z'0a637dc5a2c898a6', int64), &
                                                        int(z'113f9804bef90dae', int64), int(z'1b710b35131c471b', int64), &
                                                        int(z'28db77f523047d84', int64), int(z'32caab7b40c72493', int64), &
                                                        int(z'3c9ebe0a15c9bebc', int64), int(z'431d67c49c100d4c', int64), &
                                                        int(z'4cc5d4becb3e42b6', int64), int(z'597f299cfc657e2a', int64), &
                                                        int(z'5fcb6fab3ad6faec', int64), int(z'6c44198c4a475817', int64)]

    !> The state of a SHA-512 computation, or of a SHA-384 one when made
    !> as `sha512_context_type(state=SHA384_INITIAL_STATE,
    !> digest_size=SHA384_DIGEST_SIZE)'.  A new object starts a SHA-512
    !> computation, with the initial values of FIPS 180-2
    !> (sha512.c:sha512_init_ctx).  Feed it with `call
    !> context%process_bytes(data)' and get the digest with `call
    !> context%finish(digest)'.  Port of sha512.h:struct sha512_ctx.
    type, public :: sha512_context_type
        !> The eight chaining values.
        integer(int64) :: state(8) = [int(z'6a09e667f3bcc908', int64), int(z'bb67ae8584caa73b', int64), &
                                      int(z'3c6ef372fe94f82b', int64), int(z'a54ff53a5f1d36f1', int64), &
                                      int(z'510e527fade682d1', int64), int(z'9b05688c2b3e6c1f', int64), &
                                      int(z'1f83d9abfb41bd6b', int64), int(z'5be0cd19137e2179', int64)]
        !> The size of the digest `finish' gives, in bytes.
        integer :: digest_size = SHA512_DIGEST_SIZE
        !> The number of bytes processed so far (upstream's total, a
        !> 128-bit count; int64 holds any length Fortmacs can hold).
        integer(int64) :: total = 0
        !> The bytes of an incomplete block, waiting for more.
        character(len=SHA512_BLOCK_SIZE) :: buffer = ""
        !> The number of bytes in `buffer' (upstream's buflen).
        integer :: buffer_length = 0
    contains
        procedure :: process_bytes => process_sha512_bytes
        procedure :: finish => finish_sha512
    end type sha512_context_type

    public :: compute_sha384, compute_sha512

contains

    !> The SHA-384 digest of the bytes of DATA.  Port of
    !> sha512.c:sha384_buffer.
    pure function compute_sha384(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=SHA384_DIGEST_SIZE) :: digest
        type(sha512_context_type) :: context
        character(len=:), allocatable :: full

        context = sha512_context_type(state=SHA384_INITIAL_STATE, digest_size=SHA384_DIGEST_SIZE)
        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_sha384

    !> The SHA-512 digest of the bytes of DATA.  Port of
    !> sha512.c:sha512_buffer.
    pure function compute_sha512(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=SHA512_DIGEST_SIZE) :: digest
        type(sha512_context_type) :: context
        character(len=:), allocatable :: full

        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_sha512

    !> Add the bytes of DATA to the computation SELF: whole blocks are
    !> processed at once, the rest kept for the next call or `finish'.
    !> Port of sha512.c:sha512_process_bytes (any alignment).
    pure subroutine process_sha512_bytes(self, data)
        class(sha512_context_type), intent(inout) :: self
        character(len=*), intent(in) :: data
        integer(int64) :: i, n, add

        n = len(data, kind=int64)
        self%total = self%total + n
        i = 1

        ! When we already have some bytes in our internal buffer,
        ! concatenate both inputs first.
        if (self%buffer_length > 0) then
            add = min(int(SHA512_BLOCK_SIZE - self%buffer_length, int64), n)
            self%buffer(self%buffer_length + 1:self%buffer_length + add) = data(1:add)
            self%buffer_length = self%buffer_length + int(add)
            i = add + 1
            if (self%buffer_length < SHA512_BLOCK_SIZE) return
            call process_sha512_block(self%state, self%buffer)
            self%buffer_length = 0
        end if

        ! Process available complete blocks.
        do while (n - i + 1 >= SHA512_BLOCK_SIZE)
            call process_sha512_block(self%state, data(i:i + SHA512_BLOCK_SIZE - 1))
            i = i + SHA512_BLOCK_SIZE
        end do

        ! Move remaining bytes into the internal buffer.
        if (i <= n) then
            self%buffer(1:n - i + 1) = data(i:n)
            self%buffer_length = int(n - i + 1)
        end if
    end subroutine process_sha512_bytes

    !> Process the remaining bytes of SELF, padded as FIPS 180-2 says (a 1
    !> bit, zeros, the length in bits as 128 bits) and give the DIGEST,
    !> the first `digest_size' bytes of the chaining values in big-endian
    !> byte order.  Port of sha512.c:sha512_finish_ctx and
    !> sha384_finish_ctx (with sha512.c:sha512_conclude_ctx,
    !> sha512_read_ctx and sha384_read_ctx).
    pure subroutine finish_sha512(self, digest)
        class(sha512_context_type), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: digest
        character(len=2*SHA512_BLOCK_SIZE) :: last
        character(len=SHA512_DIGEST_SIZE) :: full
        integer(int64) :: bits, high_bits
        integer :: padded, i, k

        ! Take yet unprocessed bytes into account.
        padded = merge(SHA512_BLOCK_SIZE, 2*SHA512_BLOCK_SIZE, self%buffer_length < SHA512_BLOCK_SIZE - 16)
        last = repeat(achar(0), 2*SHA512_BLOCK_SIZE)
        last(1:self%buffer_length) = self%buffer(1:self%buffer_length)
        last(self%buffer_length + 1:self%buffer_length + 1) = char(128)
        ! Put the 128-bit file length in *bits* at the end of the buffer.
        bits = ishft(self%total, 3)
        high_bits = ishft(self%total, -61)

        do k = 0, 7
            last(padded - k:padded - k) = char(ibits(bits, 8*k, 8))
            last(padded - 8 - k:padded - 8 - k) = char(ibits(high_bits, 8*k, 8))
        end do

        ! Process last bytes.
        call process_sha512_block(self%state, last(1:SHA512_BLOCK_SIZE))
        if (padded > SHA512_BLOCK_SIZE) call process_sha512_block(self%state, last(SHA512_BLOCK_SIZE + 1:padded))
        self%buffer_length = 0

        do i = 1, 8
            do k = 0, 7
                full(8*i - k:8*i - k) = char(ibits(self%state(i), 8*k, 8))
            end do
        end do

        digest = full(1:self%digest_size)
    end subroutine finish_sha512

    !> Process the 128 bytes of BLOCK into the chaining values STATE: 80
    !> steps over its 16 big-endian words, expanded (FIPS 180-2, 6.3.2).
    !> Port of sha512.c:sha512_process_block (one block per call; the
    !> steps as a loop).
    pure subroutine process_sha512_block(state, block)
        integer(int64), intent(inout) :: state(8)
        character(len=SHA512_BLOCK_SIZE), intent(in) :: block
        integer(int64) :: x(0:79), v(8), t0, t1, s0, s1
        integer :: i, k

        do i = 0, 15
            x(i) = 0

            do k = 0, 7
                x(i) = ior(ishft(x(i), 8), int(iachar(block(8*i + k + 1:8*i + k + 1)), int64))
            end do
        end do

        do i = 16, 79
            ! S0 and S1 of sha512.c.
            s0 = ieor(ieor(ishftc(x(i - 15), -1), ishftc(x(i - 15), -8)), ishft(x(i - 15), -7))
            s1 = ieor(ieor(ishftc(x(i - 2), -19), ishftc(x(i - 2), -61)), ishft(x(i - 2), -6))
            x(i) = add_unsigned(add_unsigned(s1, x(i - 7)), add_unsigned(s0, x(i - 16)))
        end do

        v = state

        do i = 0, 79
            ! SS0(A) + F2(A, B, C).
            t0 = add_unsigned(ieor(ieor(ishftc(v(1), -28), ishftc(v(1), -34)), ishftc(v(1), -39)), &
                              ior(iand(v(1), v(2)), iand(v(3), ior(v(1), v(2)))))
            ! H + SS1(E) + F1(E, F, G) + K + M.
            t1 = add_unsigned(add_unsigned(v(8), ieor(ieor(ishftc(v(5), -14), ishftc(v(5), -18)), ishftc(v(5), -41))), &
                              add_unsigned(ieor(v(7), iand(v(5), ieor(v(6), v(7)))), &
                                           add_unsigned(ROUND_CONSTANTS(i + 1), x(i))))
            v(2:8) = v(1:7)
            v(5) = add_unsigned(v(5), t1)
            v(1) = add_unsigned(t0, t1)
        end do

        state = add_unsigned(state, v)
    end subroutine process_sha512_block

    !> The sum of the unsigned 64-bit words A and B (int64 bit patterns)
    !> modulo 2**64, added as two 32-bit halves so that no signed
    !> arithmetic overflows.  Port of u64.h:u64plus.
    elemental integer(int64) function add_unsigned(a, b) result(sum)
        integer(int64), intent(in) :: a, b
        integer(int64) :: low, high

        low = iand(a, MASK32) + iand(b, MASK32)
        high = ishft(a, -32) + ishft(b, -32) + ishft(low, -32)
        sum = ior(ishft(high, 32), iand(low, MASK32))
    end function add_unsigned
end module sha512_mod
