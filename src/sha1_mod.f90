! sha1_mod.f90 -- the SHA-1 message digest.
! Ported from GNU Emacs 31.1 lib/sha1.c.
!
! Copyright (C) 2000-2001, 2003-2006, 2008-2026 Free Software Foundation, Inc.
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

!> The SHA-1 message digest of FIPS 180-1 (Gnulib's sha1.c, which fns.c's
!> `secure_hash', `sha1' and `buffer_hash' use).  A digest is a string of
!> SHA1_DIGEST_SIZE bytes; the 32-bit words of the algorithm are kept in
!> int64 variables masked to their low 32 bits, Fortran having no
!> unsigned type.
module sha1_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none(type, external)
    private

    !> The size of a SHA-1 digest in bytes (sha1.h:SHA1_DIGEST_SIZE).
    integer, parameter, public :: SHA1_DIGEST_SIZE = 20
    !> The size of the blocks SHA-1 processes, in bytes.
    integer, parameter, public :: SHA1_BLOCK_SIZE = 64

    !> The low 32 bits of an int64.
    integer(int64), parameter :: MASK32 = int(z'ffffffff', int64)
    !> The constants of the four rounds (sha1.c:K1 to K4).
    integer(int64), parameter :: ROUND_CONSTANTS(4) = [int(z'5a827999', int64), int(z'6ed9eba1', int64), &
                                                       int(z'8f1bbcdc', int64), int(z'ca62c1d6', int64)]

    !> The state of a SHA-1 computation.  A new object starts one, with
    !> the initial values of FIPS 180-1 (sha1.c:sha1_init_ctx).  Feed it
    !> with `call context%process_bytes(data)' and get the digest with
    !> `call context%finish(digest)'.  Port of sha1.h:struct sha1_ctx.
    type, public :: sha1_context_type
        !> The chaining values A, B, C, D and E.
        integer(int64) :: state(5) = [int(z'67452301', int64), int(z'efcdab89', int64), int(z'98badcfe', int64), &
                                      int(z'10325476', int64), int(z'c3d2e1f0', int64)]
        !> The number of bytes processed so far (upstream's total).
        integer(int64) :: total = 0
        !> The bytes of an incomplete block, waiting for more.
        character(len=SHA1_BLOCK_SIZE) :: buffer = ""
        !> The number of bytes in `buffer' (upstream's buflen).
        integer :: buffer_length = 0
    contains
        procedure :: process_bytes => process_sha1_bytes
        procedure :: finish => finish_sha1
    end type sha1_context_type

    public :: compute_sha1

contains

    !> The SHA-1 digest of the bytes of DATA.  Port of sha1.c:sha1_buffer.
    pure function compute_sha1(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=SHA1_DIGEST_SIZE) :: digest
        type(sha1_context_type) :: context
        character(len=:), allocatable :: full

        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_sha1

    !> Add the bytes of DATA to the computation SELF: whole blocks are
    !> processed at once, the rest kept for the next call or `finish'.
    !> Port of sha1.c:sha1_process_bytes (any alignment).
    pure subroutine process_sha1_bytes(self, data)
        class(sha1_context_type), intent(inout) :: self
        character(len=*), intent(in) :: data
        integer(int64) :: i, n, add

        n = len(data, kind=int64)
        self%total = self%total + n
        i = 1

        ! When we already have some bytes in our internal buffer,
        ! concatenate both inputs first.
        if (self%buffer_length > 0) then
            add = min(int(SHA1_BLOCK_SIZE - self%buffer_length, int64), n)
            self%buffer(self%buffer_length + 1:self%buffer_length + add) = data(1:add)
            self%buffer_length = self%buffer_length + int(add)
            i = add + 1
            if (self%buffer_length < SHA1_BLOCK_SIZE) return
            call process_sha1_block(self%state, self%buffer)
            self%buffer_length = 0
        end if

        ! Process available complete blocks.
        do while (n - i + 1 >= SHA1_BLOCK_SIZE)
            call process_sha1_block(self%state, data(i:i + SHA1_BLOCK_SIZE - 1))
            i = i + SHA1_BLOCK_SIZE
        end do

        ! Move remaining bytes into the internal buffer.
        if (i <= n) then
            self%buffer(1:n - i + 1) = data(i:n)
            self%buffer_length = int(n - i + 1)
        end if
    end subroutine process_sha1_bytes

    !> Process the remaining bytes of SELF, padded as FIPS 180-1 says (a 1
    !> bit, zeros, the length in bits as 64 bits) and give the DIGEST, the
    !> chaining values in big-endian byte order.  Port of
    !> sha1.c:sha1_finish_ctx (and sha1.c:sha1_read_ctx).
    pure subroutine finish_sha1(self, digest)
        class(sha1_context_type), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: digest
        character(len=2*SHA1_BLOCK_SIZE) :: last
        integer(int64) :: bits
        integer :: padded, i, k

        allocate (character(len=SHA1_DIGEST_SIZE) :: digest)
        ! Take yet unprocessed bytes into account.
        padded = merge(SHA1_BLOCK_SIZE, 2*SHA1_BLOCK_SIZE, self%buffer_length < SHA1_BLOCK_SIZE - 8)
        last = repeat(achar(0), 2*SHA1_BLOCK_SIZE)
        last(1:self%buffer_length) = self%buffer(1:self%buffer_length)
        last(self%buffer_length + 1:self%buffer_length + 1) = char(128)
        ! Put the 64-bit file length in *bits* at the end of the buffer.
        bits = ishft(self%total, 3)

        do k = 0, 7
            last(padded - k:padded - k) = char(ibits(bits, 8*k, 8))
        end do

        ! Process last bytes.
        call process_sha1_block(self%state, last(1:SHA1_BLOCK_SIZE))
        if (padded > SHA1_BLOCK_SIZE) call process_sha1_block(self%state, last(SHA1_BLOCK_SIZE + 1:padded))
        self%buffer_length = 0

        do i = 1, 5
            do k = 0, 3
                digest(4*i - k:4*i - k) = char(ibits(self%state(i), 8*k, 8))
            end do
        end do
    end subroutine finish_sha1

    !> Process the 64 bytes of BLOCK into the chaining values STATE: 80
    !> steps over its 16 big-endian words, expanded (FIPS 180-1, 7).
    !> Port of sha1.c:sha1_process_block (one block per call; the steps as a
    !> loop).
    pure subroutine process_sha1_block(state, block)
        integer(int64), intent(inout) :: state(5)
        character(len=SHA1_BLOCK_SIZE), intent(in) :: block
        integer(int64) :: x(0:79), a, b, c, d, e, f, t
        integer :: i, k, round

        do i = 0, 15
            x(i) = 0

            do k = 0, 3
                x(i) = ior(ishft(x(i), 8), int(iachar(block(4*i + k + 1:4*i + k + 1)), int64))
            end do
        end do

        do i = 16, 79
            x(i) = ishftc(ieor(ieor(x(i - 3), x(i - 8)), ieor(x(i - 14), x(i - 16))), 1, 32)
        end do

        a = state(1)
        b = state(2)
        c = state(3)
        d = state(4)
        e = state(5)

        do round = 1, 4
            do i = 20*round - 20, 20*round - 1
                select case (round)
                case (1)
                    ! F1: d ^ (b & (c ^ d)).
                    f = ieor(d, iand(b, ieor(c, d)))
                case (3)
                    ! F3: (b & c) | (d & (b | c)).
                    f = ior(iand(b, c), iand(d, ior(b, c)))
                case default
                    ! F2 and F4: b ^ c ^ d.
                    f = ieor(b, ieor(c, d))
                end select

                t = iand(ishftc(a, 5, 32) + f + e + ROUND_CONSTANTS(round) + x(i), MASK32)
                e = d
                d = c
                c = ishftc(b, 30, 32)
                b = a
                a = t
            end do
        end do

        state(1) = iand(state(1) + a, MASK32)
        state(2) = iand(state(2) + b, MASK32)
        state(3) = iand(state(3) + c, MASK32)
        state(4) = iand(state(4) + d, MASK32)
        state(5) = iand(state(5) + e, MASK32)
    end subroutine process_sha1_block
end module sha1_mod
