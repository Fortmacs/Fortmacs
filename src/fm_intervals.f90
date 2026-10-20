! fm_intervals.f90 -- the intervals that carry text properties.
! Ported from GNU Emacs 31.1 src/intervals.c and src/intervals.h.
!
! Copyright (C) 1993-1995, 1997-1998, 2001-2026 Free Software
! Foundation, Inc.
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

!> A text's intervals: maximal runs of characters, each with a property
!> list.  When a text has any, they cover it exactly.
!>
!> Representation.  Upstream keeps intervals in a balanced binary tree with
!> parent links.  Here they are an array of runs sorted by position, so
!> "next_interval" is k + 1.  Lookups are a binary search; edits shift the
!> runs after them, which is O(number of runs).  The interface follows
!> intervals.c so the tree can be swapped in later if profiling asks for it.
!>
!> Interval structure is not observable except through
!> `next-property-change` with LIMIT t, as upstream.
module fm_intervals
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_lisp
   use fm_lread, only: intern, ensure_obarray
   use fm_data, only: value_eq
   implicit none(type, external)
   private

   !> Port of intervals.h:struct interval (position, length, plist).
   type, public :: interval_type
      integer(int64) :: position = 1
      integer(int64) :: length = 0
      type(value_type) :: plist
   end type interval_type

   !> All intervals of one text, in order.  `n` == 0 means "no intervals"
   !> (upstream: a NULL tree).
   type, public :: intervals_type
      type(interval_type), allocatable :: runs(:)
      integer(int64) :: n = 0
   end type intervals_type

   public :: create_root_interval, find_interval, interval_end, &
             split_interval_right, split_interval_left, intervals_equal, &
             offset_intervals, textget, plist_get_eq, tmem

contains

   !> Make `iv` one interval covering `length` characters from `beg`.
   !> Port of intervals.c:create_root_interval.
   pure subroutine create_root_interval(iv, beg, length)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: beg, length

      if (allocated(iv%runs)) deallocate (iv%runs)
      allocate (iv%runs(8))
      iv%n = 1
      iv%runs(1)%position = beg
      iv%runs(1)%length = length
   end subroutine create_root_interval

   !> Position just past interval `k`.
   elemental integer(int64) function interval_end(iv, k)
      type(intervals_type), intent(in) :: iv
      integer(int64), intent(in) :: k

      interval_end = iv%runs(k)%position + iv%runs(k)%length
   end function interval_end

   !> Index of the interval containing `pos` (the last one when `pos` is
   !> the end of the text).  Port of intervals.c:find_interval.
   pure integer(int64) function find_interval(iv, pos) result(k)
      type(intervals_type), intent(in) :: iv
      integer(int64), intent(in) :: pos
      integer(int64) :: lo, hi, mid

      lo = 1
      hi = iv%n
      do while (lo < hi)
         mid = (lo + hi + 1)/2
         if (iv%runs(mid)%position <= pos) then
            lo = mid
         else
            hi = mid - 1
         end if
      end do
      k = lo
   end function find_interval

   !> Make room for a run at index `k` (shifting k..n up by one).
   pure subroutine open_slot(iv, k)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: k
      type(interval_type), allocatable :: grown(:)
      integer(int64) :: j

      if (iv%n == size(iv%runs, kind=int64)) then
         allocate (grown(2*iv%n))
         do j = 1, iv%n
            grown(j) = iv%runs(j)
         end do
         call move_alloc(grown, iv%runs)
      end if
      do j = iv%n, k, -1
         iv%runs(j + 1) = iv%runs(j)
      end do
      iv%n = iv%n + 1
   end subroutine open_slot

   !> Remove run `k`.
   pure subroutine close_slot(iv, k)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: k
      integer(int64) :: j

      do j = k, iv%n - 1
         iv%runs(j) = iv%runs(j + 1)
      end do
      iv%runs(iv%n) = interval_type()
      iv%n = iv%n - 1
   end subroutine close_slot

   !> Split interval `k` `offset` characters in; return the index of the
   !> new right-hand part.  Both parts keep the plist (upstream callers
   !> copy it with copy_properties).  Port of intervals.c:split_interval_right.
   pure subroutine split_interval_right(iv, k, offset, r)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: k, offset
      integer(int64), intent(out) :: r

      call open_slot(iv, k + 1)
      r = k + 1
      iv%runs(r)%plist = iv%runs(k)%plist
      iv%runs(r)%position = iv%runs(k)%position + offset
      iv%runs(r)%length = iv%runs(k)%length - offset
      iv%runs(k)%length = offset
   end subroutine split_interval_right

   !> Split interval `k` so that its first `length` characters become their
   !> own interval; return the index of that left part (`k`).
   !> Port of intervals.c:split_interval_left.
   pure subroutine split_interval_left(iv, k, length, l)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: k, length
      integer(int64), intent(out) :: l
      integer(int64) :: r

      call split_interval_right(iv, k, length, r)
      l = r - 1
   end subroutine split_interval_left

   !> Value of `prop` in `plist` compared with eq, and whether it is there.
   !> Port of fns.c:plist_get (also property_value's role in textprop.c).
   subroutine plist_get_eq(plist, prop, val, found)
      type(value_type), intent(in) :: plist, prop
      type(value_type), intent(out) :: val
      logical, intent(out) :: found
      type(value_type), allocatable :: elems(:)
      integer(int64) :: k

      found = .false.
      call list_elements(plist, elems)
      do k = 1, size(elems, kind=int64) - 1, 2
         if (value_eq(elems(k), prop)) then
            val = elems(k + 1)
            found = .true.
            return
         end if
      end do
   end subroutine plist_get_eq

   !> Value of text property `prop` in `plist`, falling back to the
   !> `category` symbol's property.  Port of intervals.c:textget /
   !> lookup_char_property (char-property-alias-alist and
   !> default-text-properties are not consulted yet).
   function textget(plist, prop) result(val)
      type(value_type), intent(in) :: plist, prop
      type(value_type) :: val
      type(value_type), allocatable :: elems(:)
      type(value_type) :: category, fallback, tem
      logical :: found
      integer(int64) :: k

      category = make_symbol_ref(intern("category"))
      call list_elements(plist, elems)
      do k = 1, size(elems, kind=int64) - 1, 2
         if (value_eq(elems(k), prop)) then
            val = elems(k + 1)
            return
         end if
         if (value_eq(elems(k), category)) then
            tem = elems(k + 1)
            if (symbolp(tem)) then
               call ensure_obarray()
               call plist_get_eq(symbols(xsymbol(tem))%plist, prop, fallback, found)
            end if
         end if
      end do
      val = fallback
   end function textget

   !> True if `sym` is in list `set`, or `set` is a non-nil non-list.
   !> Port of textprop.c:TMEM.
   logical function tmem(sym, set)
      type(value_type), intent(in) :: sym, set
      type(value_type), allocatable :: elems(:)
      integer(int64) :: k

      if (consp(set)) then
         tmem = .false.
         call list_elements(set, elems)
         do k = 1, size(elems, kind=int64)
            if (value_eq(elems(k), sym)) then
               tmem = .true.
               return
            end if
         end do
      else
         tmem = .not. nilp(set)
      end if
   end function tmem

   !> True if the two plists have the same properties with eq values.
   !> Port of intervals.c:intervals_equal (intervals_equal_1 with eq).
   logical function intervals_equal(p0, p1)
      type(value_type), intent(in) :: p0, p1
      type(value_type), allocatable :: e0(:)
      type(value_type) :: val
      logical :: found
      integer(int64) :: k

      intervals_equal = .false.
      if (list_length(p0) /= list_length(p1)) return
      if (mod(list_length(p0), 2_int64) /= 0) return
      call list_elements(p0, e0)
      do k = 1, size(e0, kind=int64) - 1, 2
         call plist_get_eq(p1, e0(k), val, found)
         if (.not. found) return
         if (.not. value_eq(val, e0(k + 1))) return
      end do
      intervals_equal = .true.
   end function intervals_equal

   !> Adjust `iv` for `length` characters inserted (length > 0) or deleted
   !> (length < 0) at `start`.  Inserted text gets its own interval with no
   !> properties; inheriting through stickiness (`insert-and-inherit`) is
   !> pending.  Port of intervals.c:offset_intervals,
   !> adjust_intervals_for_insertion and adjust_intervals_for_deletion.
   pure subroutine offset_intervals(iv, start, length)
      type(intervals_type), intent(inout) :: iv
      integer(int64), intent(in) :: start, length
      integer(int64) :: k, j, r, a, b, cut

      if (iv%n == 0 .or. length == 0) return

      if (length > 0) then
         k = find_interval(iv, start)
         if (start == interval_end(iv, k)) then
            k = k + 1                       ! append after the last run
         else if (start > iv%runs(k)%position) then
            call split_interval_right(iv, k, start - iv%runs(k)%position, r)
            k = r
         end if
         call open_slot(iv, k)
         iv%runs(k) = interval_type()
         iv%runs(k)%position = start
         iv%runs(k)%length = length
         do j = k + 1, iv%n
            iv%runs(j)%position = iv%runs(j)%position + length
         end do
      else
         a = start
         b = start - length
         j = 1
         do while (j <= iv%n)
            associate (run => iv%runs(j))
               cut = max(0_int64, min(b, run%position + run%length) - max(a, run%position))
               if (run%position >= b) then
                  run%position = run%position + length
               else if (run%position > a) then
                  run%position = a
               end if
               run%length = run%length - cut
            end associate
            if (iv%runs(j)%length == 0) then
               call close_slot(iv, j)
            else
               j = j + 1
            end if
         end do
      end if
   end subroutine offset_intervals

end module fm_intervals
