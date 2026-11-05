! md5_mod.f90 -- the MD5 message digest.
! Ported from GNU Emacs 31.1 lib/md5.c.
!
! Copyright (C) 1995-1997, 1999-2001, 2005-2006, 2008-2026 Free Software Foundation, Inc.
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

!> The MD5 message digest of RFC 1321 (Gnulib's md5.c, which fns.c's
!> `md5' and `secure_hash' use).  A digest is a string of
!> MD5_DIGEST_SIZE bytes; the 32-bit words of the algorithm are kept in
!> int64 variables masked to their low 32 bits, Fortran having no
!> unsigned type.
module md5_mod
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none(type, external)
    private

    !> The size of an MD5 digest in bytes (md5.h:MD5_DIGEST_SIZE).
    integer, parameter, public :: MD5_DIGEST_SIZE = 16
    !> The size of the blocks MD5 processes, in bytes (md5.h:MD5_BLOCK_SIZE).
    integer, parameter, public :: MD5_BLOCK_SIZE = 64

    !> The low 32 bits of an int64.
    integer(int64), parameter :: MASK32 = int(z'ffffffff', int64)
    !> The word (counting from 0) each of the 64 steps adds.
    integer, parameter :: STEP_WORDS(64) = [ &
                          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
                          1, 6, 11, 0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, &
                          5, 8, 11, 14, 1, 4, 7, 10, 13, 0, 3, 6, 9, 12, 15, 2, &
                          0, 7, 14, 5, 12, 3, 10, 1, 8, 15, 6, 13, 4, 11, 2, 9]
    !> The rotation of each of the 64 steps.
    integer, parameter :: STEP_SHIFTS(64) = [ &
                          7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, &
                          5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, &
                          4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, &
                          6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]
    !> The constant of each of the 64 steps, the integer part of
    !> 4294967296 * abs(sin(i)) for step i (RFC 1321, 3.4).
    integer(int64), parameter :: STEP_CONSTANTS(64) = [int(z'd76aa478', int64), int(z'e8c7b756', int64), &
                                                       int(z'242070db', int64), int(z'c1bdceee', int64), &
                                                       int(z'f57c0faf', int64), int(z'4787c62a', int64), &
                                                       int(z'a8304613', int64), int(z'fd469501', int64), &
                                                       int(z'698098d8', int64), int(z'8b44f7af', int64), &
                                                       int(z'ffff5bb1', int64), int(z'895cd7be', int64), &
                                                       int(z'6b901122', int64), int(z'fd987193', int64), &
                                                       int(z'a679438e', int64), int(z'49b40821', int64), &
                                                       int(z'f61e2562', int64), int(z'c040b340', int64), &
                                                       int(z'265e5a51', int64), int(z'e9b6c7aa', int64), &
                                                       int(z'd62f105d', int64), int(z'02441453', int64), &
                                                       int(z'd8a1e681', int64), int(z'e7d3fbc8', int64), &
                                                       int(z'21e1cde6', int64), int(z'c33707d6', int64), &
                                                       int(z'f4d50d87', int64), int(z'455a14ed', int64), &
                                                       int(z'a9e3e905', int64), int(z'fcefa3f8', int64), &
                                                       int(z'676f02d9', int64), int(z'8d2a4c8a', int64), &
                                                       int(z'fffa3942', int64), int(z'8771f681', int64), &
                                                       int(z'6d9d6122', int64), int(z'fde5380c', int64), &
                                                       int(z'a4beea44', int64), int(z'4bdecfa9', int64), &
                                                       int(z'f6bb4b60', int64), int(z'bebfbc70', int64), &
                                                       int(z'289b7ec6', int64), int(z'eaa127fa', int64), &
                                                       int(z'd4ef3085', int64), int(z'04881d05', int64), &
                                                       int(z'd9d4d039', int64), int(z'e6db99e5', int64), &
                                                       int(z'1fa27cf8', int64), int(z'c4ac5665', int64), &
                                                       int(z'f4292244', int64), int(z'432aff97', int64), &
                                                       int(z'ab9423a7', int64), int(z'fc93a039', int64), &
                                                       int(z'655b59c3', int64), int(z'8f0ccc92', int64), &
                                                       int(z'ffeff47d', int64), int(z'85845dd1', int64), &
                                                       int(z'6fa87e4f', int64), int(z'fe2ce6e0', int64), &
                                                       int(z'a3014314', int64), int(z'4e0811a1', int64), &
                                                       int(z'f7537e82', int64), int(z'bd3af235', int64), &
                                                       int(z'2ad7d2bb', int64), int(z'eb86d391', int64)]

    !> The state of an MD5 computation.  A new object starts one, with the
    !> initial chaining values of RFC 1321, 3.3 (md5.c:md5_init_ctx).
    !> Feed it with `call context%process_bytes(data)' and get the digest
    !> with `call context%finish(digest)'.  Port of md5.h:struct md5_ctx.
    type, public :: md5_context_type
        !> The chaining values A, B, C and D.
        integer(int64) :: state(4) = [int(z'67452301', int64), int(z'efcdab89', int64), int(z'98badcfe', int64), &
                                      int(z'10325476', int64)]
        !> The number of bytes processed so far (upstream's total).
        integer(int64) :: total = 0
        !> The bytes of an incomplete block, waiting for more.
        character(len=MD5_BLOCK_SIZE) :: buffer = ""
        !> The number of bytes in `buffer' (upstream's buflen).
        integer :: buffer_length = 0
    contains
        procedure :: process_bytes => process_md5_bytes
        procedure :: finish => finish_md5
    end type md5_context_type

    public :: compute_md5

contains

    !> The MD5 digest of the bytes of DATA.  Port of md5.c:md5_buffer.
    pure function compute_md5(data) result(digest)
        character(len=*), intent(in) :: data
        character(len=MD5_DIGEST_SIZE) :: digest
        type(md5_context_type) :: context
        character(len=:), allocatable :: full

        call context%process_bytes(data)
        call context%finish(full)
        digest = full
    end function compute_md5

    !> Add the bytes of DATA to the computation SELF: whole blocks are
    !> processed at once, the rest kept for the next call or `finish'.
    !> Port of md5.c:md5_process_bytes (any alignment).
    pure subroutine process_md5_bytes(self, data)
        class(md5_context_type), intent(inout) :: self
        character(len=*), intent(in) :: data
        integer(int64) :: i, n, add

        n = len(data, kind=int64)
        self%total = self%total + n
        i = 1

        ! When we already have some bytes in our internal buffer,
        ! concatenate both inputs first.
        if (self%buffer_length > 0) then
            add = min(int(MD5_BLOCK_SIZE - self%buffer_length, int64), n)
            self%buffer(self%buffer_length + 1:self%buffer_length + add) = data(1:add)
            self%buffer_length = self%buffer_length + int(add)
            i = add + 1
            if (self%buffer_length < MD5_BLOCK_SIZE) return
            call process_md5_block(self%state, self%buffer)
            self%buffer_length = 0
        end if

        ! Process available complete blocks.
        do while (n - i + 1 >= MD5_BLOCK_SIZE)
            call process_md5_block(self%state, data(i:i + MD5_BLOCK_SIZE - 1))
            i = i + MD5_BLOCK_SIZE
        end do

        ! Move remaining bytes into the internal buffer.
        if (i <= n) then
            self%buffer(1:n - i + 1) = data(i:n)
            self%buffer_length = int(n - i + 1)
        end if
    end subroutine process_md5_bytes

    !> Process the remaining bytes of SELF, pad them as RFC 1321, 3.1 and
    !> 3.2 say (a 1 bit, zeros, the length in bits as 64 bits) and give
    !> the DIGEST, the chaining values in little-endian byte order.
    !> Port of md5.c:md5_finish_ctx (and md5.c:md5_read_ctx).
    pure subroutine finish_md5(self, digest)
        class(md5_context_type), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: digest
        character(len=2*MD5_BLOCK_SIZE) :: last
        integer(int64) :: bits
        integer :: padded, i, k

        allocate (character(len=MD5_DIGEST_SIZE) :: digest)
        ! Take yet unprocessed bytes into account.
        padded = merge(MD5_BLOCK_SIZE, 2*MD5_BLOCK_SIZE, self%buffer_length < MD5_BLOCK_SIZE - 8)
        last = repeat(achar(0), 2*MD5_BLOCK_SIZE)
        last(1:self%buffer_length) = self%buffer(1:self%buffer_length)
        last(self%buffer_length + 1:self%buffer_length + 1) = char(128)
        ! Put the 64-bit file length in *bits* at the end of the buffer.
        bits = ishft(self%total, 3)

        do k = 0, 7
            last(padded - 7 + k:padded - 7 + k) = char(ibits(bits, 8*k, 8))
        end do

        ! Process last bytes.
        call process_md5_block(self%state, last(1:MD5_BLOCK_SIZE))
        if (padded > MD5_BLOCK_SIZE) call process_md5_block(self%state, last(MD5_BLOCK_SIZE + 1:padded))
        self%buffer_length = 0

        do i = 1, 4
            do k = 0, 3
                digest(4*i + k - 3:4*i + k - 3) = char(ibits(self%state(i), 8*k, 8))
            end do
        end do
    end subroutine finish_md5

    !> Process the 64 bytes of BLOCK into the chaining values STATE: four
    !> rounds of 16 steps over its 16 little-endian words (RFC 1321, 3.4).
    !> Port of md5.c:md5_process_block (one block per call; the steps
    !> from tables).
    pure subroutine process_md5_block(state, block)
        integer(int64), intent(inout) :: state(4)
        character(len=MD5_BLOCK_SIZE), intent(in) :: block
        integer(int64) :: words(0:15), a, b, c, d, f, t
        integer :: i, k, round

        do i = 0, 15
            words(i) = 0

            do k = 3, 0, -1
                words(i) = ior(ishft(words(i), 8), int(iachar(block(4*i + k + 1:4*i + k + 1)), int64))
            end do
        end do

        a = state(1)
        b = state(2)
        c = state(3)
        d = state(4)

        do round = 1, 4
            do i = 16*round - 15, 16*round
                select case (round)
                case (1)
                    ! FF: d ^ (b & (c ^ d)).
                    f = ieor(d, iand(b, ieor(c, d)))
                case (2)
                    ! FOREGROUND: FF(d, b, c).
                    f = ieor(c, iand(d, ieor(b, c)))
                case (3)
                    ! FH: b ^ c ^ d.
                    f = ieor(b, ieor(c, d))
                case default
                    ! FI: c ^ (b | ~d).
                    f = ieor(c, iand(ior(b, not(d)), MASK32))
                end select

                t = iand(a + f + words(STEP_WORDS(i)) + STEP_CONSTANTS(i), MASK32)
                a = d
                d = c
                c = b
                b = iand(b + ishftc(t, STEP_SHIFTS(i), 32), MASK32)
            end do
        end do

        ! Add the starting values of the context.
        state(1) = iand(state(1) + a, MASK32)
        state(2) = iand(state(2) + b, MASK32)
        state(3) = iand(state(3) + c, MASK32)
        state(4) = iand(state(4) + d, MASK32)
    end subroutine process_md5_block
end module md5_mod
