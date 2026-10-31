! intervals_fm.f90 -- the intervals that carry text properties.
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
!> `next_property_change' with LIMIT t, as upstream.
module intervals_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: consp, list_elements, list_length, make_symbol_ref, symbolp, symbols, make_list, &
                       value_type, xsymbol, string_intervals, set_string_intervals
    use lread_fm, only: intern, ensure_obarray
    use data_fm, only: value_eq
    implicit none(type, external)
    private

    !> Port of intervals.h:struct interval (position, length, plist).
    type, public :: interval_type
        integer(int64) :: position = 1
        integer(int64) :: length = 0
        type(value_type) :: plist
    end type interval_type

    !> All intervals of one text, in order.  `n' == 0 means "no intervals"
    !> (upstream: a NULL tree).
    type, public :: intervals_type
        type(interval_type), allocatable :: runs(:)
        integer(int64) :: n = 0
    end type intervals_type

    public :: create_root_interval, find_interval, interval_end, &
              split_interval_right, split_interval_left, intervals_equal, &
              offset_intervals, textget, plist_get_eq, tmem, default_nonsticky, merge_properties, &
              merge_properties_sticky, insertion_plist, graft_intervals_into_buffer, copy_intervals, &
              compare_string_intervals, copy_intervals_to_string

contains

    !> Make `iv' one interval covering `length' characters from `beg'.
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

    !> Position just past interval `k'.  Not a port.
    elemental integer(int64) function interval_end(iv, k)
        type(intervals_type), intent(in) :: iv
        integer(int64), intent(in) :: k

        interval_end = iv%runs(k)%position + iv%runs(k)%length
    end function interval_end

    !> Index of the interval containing `pos' (the last one when `pos' is
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

    !> Make room for a run at index `k' (shifting k..n up by one).
    !> Not a port.
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

    !> Remove run `k'.  Not a port.
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

    !> Split interval `k' `offset' characters in; return the index of the
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

    !> Split interval `k' so that its first `length' characters become their
    !> own interval; return the index of that left part (`k').
    !> Port of intervals.c:split_interval_left.
    pure subroutine split_interval_left(iv, k, length, l)
        type(intervals_type), intent(inout) :: iv
        integer(int64), intent(in) :: k, length
        integer(int64), intent(out) :: l
        integer(int64) :: r

        call split_interval_right(iv, k, length, r)
        l = r - 1
    end subroutine split_interval_left

    !> Value of `prop' in `plist' compared with eq, and whether it is there.
    !> Port of fns.c:plist_get and textprop.c:property_value.
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

    !> Value of text property `prop' in `plist', falling back to the
    !> `category' symbol's property, then to the first of the properties
    !> `char_property_alias_table' gives as alternatives for `prop'.
    !> Port of intervals.c:textget and intervals.c:lookup_char_property
    !> (default-text-properties is not consulted yet).
    function textget(plist, prop) result(val)
        type(value_type), intent(in) :: plist, prop
        type(value_type) :: val
        type(value_type), allocatable :: elems(:), aliases(:), alias(:)
        type(value_type) :: category, fallback, tem
        logical :: found
        integer(int64) :: k, j, id

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
        if (allocated(fallback%a)) return
        ! Check for alternative properties.
        call ensure_obarray()
        id = intern("char_property_alias_table")
        if (.not. symbols(id)%bound) return
        if (.not. consp(symbols(id)%val)) return
        call list_elements(symbols(id)%val, aliases)

        do k = 1, size(aliases, kind=int64)
            if (.not. consp(aliases(k))) cycle
            call list_elements(aliases(k), alias)
            if (.not. value_eq(alias(1), prop)) cycle

            do j = 2, size(alias, kind=int64)
                call plist_get_eq(plist, alias(j), val, found)
                if (allocated(val%a)) return
            end do

            return
        end do
    end function textget

    !> True if `sym' is in list `set', or `set' is a non-nil non-list.
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
            tmem = allocated(set%a)
        end if
    end function tmem

    !> True if the two plists have the same properties with eq values.
    !> Port of intervals.c:intervals_equal and intervals.c:intervals_equal_1
    !> (with eq).
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

    !> Adjust `iv' for `length' characters inserted (length > 0) or deleted
    !> (length < 0) at `start'.  Inserted text gets its own interval: with no
    !> properties, or with INHERIT true those `insertion_plist' gives it
    !> from its surroundings (upstream's adjust_intervals_for_insertion
    !> puts it in a neighboring interval by stickiness).  Port of
    !> intervals.c:offset_intervals, intervals.c:adjust_intervals_for_insertion
    !> and intervals.c:adjust_intervals_for_deletion.
    subroutine offset_intervals(iv, start, length, inherit)
        type(intervals_type), intent(inout) :: iv
        integer(int64), intent(in) :: start, length
        logical, intent(in), optional :: inherit
        integer(int64) :: k, j, r, a, b, cut
        type(value_type) :: plist

        if (iv%n == 0 .or. length == 0) return

        if (length > 0) then
            plist = value_type()

            if (present(inherit)) then
                if (inherit) plist = insertion_plist(iv, start)
            end if

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
            iv%runs(k)%plist = plist

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

    ! ------------------------------------------------------------------
    ! Stickiness, copying and grafting
    ! ------------------------------------------------------------------

    !> Whether `text_property_default_nonsticky' makes PROP nonsticky: its
    !> standard value makes `syntax_table' and `display' (textprop.c),
    !> `composition' (composite.c) and `fill_space' (fill.el)
    !> rear-nonsticky.  Not a port: the value is built in (its table needs
    !> dotted pairs).
    logical function default_nonsticky(prop)
        type(value_type), intent(in) :: prop

        default_nonsticky = value_eq(prop, qsym("fill_space")) .or. value_eq(prop, qsym("composition")) .or. &
                            value_eq(prop, qsym("syntax_table")) .or. value_eq(prop, qsym("display"))
    end function default_nonsticky

    ! Not a port.
    function qsym(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v

        call ensure_obarray()
        v = make_symbol_ref(intern(name))
    end function qsym

    !> Whether PROP is one of the properties of PLIST.  Not a port.
    logical function plist_member(plist, prop)
        type(value_type), intent(in) :: plist, prop
        type(value_type) :: val

        call plist_get_eq(plist, prop, val, plist_member)
    end function plist_member

    !> Merge the properties of SOURCE into TARGET: each property of SOURCE
    !> is added to TARGET if TARGET has no such property as yet.  Port of
    !> intervals.c:merge_properties.
    subroutine merge_properties(source, target)
        type(value_type), intent(in) :: source
        type(value_type), intent(inout) :: target
        type(value_type), allocatable :: s(:), t(:), merged(:)
        integer(int64) :: k, n

        if (.not. allocated(source%a)) return
        call list_elements(source, s)
        call list_elements(target, t)
        allocate (merged(size(s) + size(t)))
        n = 0

        do k = 1, size(s, kind=int64) - 1, 2
            if (plist_member(target, s(k))) cycle
            merged(n + 1) = s(k)
            merged(n + 2) = s(k + 1)
            n = n + 2
        end do

        if (n == 0) return

        ! New properties go in front, as upstream conses them on.
        do k = 1, size(t, kind=int64)
            merged(n + k) = t(k)
        end do

        target = make_list(merged(1:n + size(t, kind=int64)))
    end subroutine merge_properties

    !> Merge the properties PLEFT (of the text before an insertion) and
    !> PRIGHT (of the text after it) into the properties the inserted text
    !> gets, according to their stickiness: `front_sticky' and
    !> `rear_nonsticky' (lists of properties, or t for all), and
    !> `text_property_default_nonsticky' for the rest.  Port of
    !> intervals.c:merge_properties_sticky.
    function merge_properties_sticky(pleft, pright) result(props)
        type(value_type), intent(in) :: pleft, pright
        type(value_type) :: props
        type(value_type), allocatable :: left(:), right(:), out(:), front(:), rear(:), with_front(:)
        type(value_type) :: lfront, lrear, rfront, rrear, sym, lval, rval, cat, tem
        logical :: use_left, use_right, lpresent, nonsticky, found
        integer(int64) :: k, j, n, nf, nr

        lfront = textget(pleft, qsym("front_sticky"))
        lrear = textget(pleft, qsym("rear_nonsticky"))
        rfront = textget(pright, qsym("front_sticky"))
        rrear = textget(pright, qsym("rear_nonsticky"))
        call list_elements(pleft, left)
        call list_elements(pright, right)
        allocate (out(size(left) + size(right) + 4), front(size(left) + size(right)), rear(size(left) + size(right)))
        n = 0
        nf = 0
        nr = 0

        ! Go through each element of PRIGHT.
        do k = 1, size(right, kind=int64) - 1, 2
            sym = right(k)
            ! Sticky properties get special treatment.
            if (value_eq(sym, qsym("rear_nonsticky")) .or. value_eq(sym, qsym("front_sticky"))) cycle
            rval = right(k + 1)
            ! Whether the property is explicitly defined on the left.
            call plist_get_eq(pleft, sym, lval, lpresent)
            if (.not. lpresent) lval = value_type()
            ! Even if lrear or rfront say nothing about the stickiness of
            ! SYM, text_property_default_nonsticky may give default
            ! stickiness to SYM.
            nonsticky = default_nonsticky(sym)
            use_left = lpresent .and. .not. (tmem(sym, lrear) .or. nonsticky)
            use_right = tmem(sym, rfront)

            if (use_left .and. use_right) then
                if (.not. allocated(lval%a)) then
                    use_left = .false.
                else if (.not. allocated(rval%a)) then
                    use_right = .false.
                end if
            end if

            if (use_left) then
                call push_prop(sym, lval)
                if (tmem(sym, lfront)) call push_front(sym)
                if (tmem(sym, lrear)) call push_rear(sym)
            else if (use_right) then
                call push_prop(sym, rval)
                if (tmem(sym, rfront)) call push_front(sym)
                if (tmem(sym, rrear)) call push_rear(sym)
            end if
        end do

        ! Now go through each element of PLEFT.
        do k = 1, size(left, kind=int64) - 1, 2
            sym = left(k)
            ! Sticky properties get special treatment.
            if (value_eq(sym, qsym("rear_nonsticky")) .or. value_eq(sym, qsym("front_sticky"))) cycle
            ! If sym is in PRIGHT, we've already considered it.
            if (plist_member(pright, sym)) cycle
            lval = left(k + 1)
            nonsticky = default_nonsticky(sym)

            ! Since rval is known to be nil in this loop, the test
            ! simplifies.
            if (.not. (tmem(sym, lrear) .or. nonsticky)) then
                call push_prop(sym, lval)
                if (tmem(sym, lfront)) call push_front(sym)
            else if (tmem(sym, rfront)) then
                ! The value is nil, but we still inherit the stickiness
                ! from the right.
                call push_front(sym)
                if (tmem(sym, rrear)) call push_rear(sym)
            end if
        end do

        if (nr > 0) then
            do j = n, 1, -1
                out(j + 2) = out(j)
            end do

            out(1) = qsym("rear_nonsticky")
            out(2) = make_list(rear(1:nr))
            n = n + 2
        end if

        if (n > 0) then
            props = make_list(out(1:n))
        else
            props = value_type()
        end if

        cat = textget(props, qsym("category"))

        if (nf > 0) then
            ! If we have inherited a front-stick category property that is
            ! t, we don't need to set up a detailed one.
            found = .false.

            if (allocated(cat%a) .and. symbolp(cat)) then
                call ensure_obarray()
                call plist_get_eq(symbols(xsymbol(cat))%plist, qsym("front_sticky"), tem, found)
                found = found .and. value_eq(tem, qsym("t"))
            end if

            if (.not. found) then
                call list_elements(props, out)
                allocate (with_front(size(out) + 2))
                with_front(1) = qsym("front_sticky")
                with_front(2) = make_list(front(1:nf))

                do j = 1, size(out, kind=int64)
                    with_front(j + 2) = out(j)
                end do

                props = make_list(with_front)
            end if
        end if

    contains

        subroutine push_prop(s, v)
            type(value_type), intent(in) :: s, v

            out(n + 1) = s
            out(n + 2) = v
            n = n + 2
        end subroutine push_prop

        subroutine push_front(s)
            type(value_type), intent(in) :: s

            nf = nf + 1
            front(nf) = s
        end subroutine push_front

        subroutine push_rear(s)
            type(value_type), intent(in) :: s

            nr = nr + 1
            rear(nr) = s
        end subroutine push_rear
    end function merge_properties_sticky

    !> The properties that text inserted at POSITION of the text with
    !> intervals IV gets from the text around it: if POSITION is inside an
    !> interval none of whose properties is rear-nonsticky, that
    !> interval's; else those `merge_properties_sticky' gives from the
    !> intervals before and after POSITION.  Port of
    !> intervals.c:adjust_intervals_for_insertion (the properties it gives
    !> the inserted text; the tree adjustment is `offset_intervals').
    function insertion_plist(iv, position) result(plist)
        type(intervals_type), intent(in) :: iv
        integer(int64), intent(in) :: position
        type(value_type) :: plist
        type(value_type) :: front, rear, pleft, pright
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k, pos
        logical :: is_eob, problem

        plist = value_type()
        if (iv%n == 0) return
        pos = position
        is_eob = .false.

        ! If inserting at point-max of a buffer, that position will be out
        ! of range.
        if (pos >= interval_end(iv, iv%n)) then
            pos = interval_end(iv, iv%n)
            is_eob = .true.
        end if

        k = find_interval(iv, pos)
        pleft = value_type()
        pright = value_type()

        ! If in middle of an interval which is not sticky either way, we
        ! must not just give its properties to the insertion.
        if (.not. (pos == iv%runs(k)%position .or. is_eob)) then
            rear = textget(iv%runs(k)%plist, qsym("rear_nonsticky"))
            front = textget(iv%runs(k)%plist, qsym("front_sticky"))

            if (.not. consp(rear) .and. allocated(rear%a)) then
                ! All properties are nonsticky.  We split the interval.
                problem = .true.
            else if (.not. consp(front) .and. allocated(front%a)) then
                ! All properties are sticky.  We don't split the interval.
                problem = .false.
            else
                ! Does any actual property pose an actual problem?
                problem = .false.
                call list_elements(iv%runs(k)%plist, elems)

                do pos = 1, size(elems, kind=int64) - 1, 2
                    ! Is this particular property front-sticky?
                    if (consp(front) .and. tmem(elems(pos), front)) cycle

                    ! Is this particular property rear-nonsticky?
                    if ((consp(rear) .and. tmem(elems(pos), rear)) .or. default_nonsticky(elems(pos))) then
                        problem = .true.
                        exit
                    end if
                end do
            end if

            if (.not. problem) then
                plist = iv%runs(k)%plist
                return
            end if

            ! Split: the text then lies between two halves of this interval.
            pleft = iv%runs(k)%plist
            pright = iv%runs(k)%plist
        else
            ! Between intervals: check the stickiness of both of them.
            if (is_eob) then
                pleft = iv%runs(k)%plist
            else
                if (k > 1) pleft = iv%runs(k - 1)%plist
                pright = iv%runs(k)%plist
            end if
        end if

        plist = merge_properties_sticky(pleft, pright)
    end function insertion_plist

    !> Graft the text properties of inserted text, given as pieces
    !> [STARTS(k), ENDS(k)) relative to the start of the text with
    !> properties PLISTS(k) (pieces left out have none), into the intervals
    !> IV of a buffer whose text ends at Z, where the LENGTH characters at
    !> POSITION were inserted.  INHERIT true means the inserted text keeps
    !> the properties it got from its surroundings, its own ones added
    !> where it has none (else its own replace them).  With no pieces and
    !> no INHERIT the inserted text gets no properties.  Port of
    !> intervals.c:graft_intervals_into_buffer (with
    !> intervals.c:copy_properties, a plist assignment here).
    subroutine graft_intervals_into_buffer(iv, z, starts, ends, plists, position, length, inherit)
        type(intervals_type), intent(inout) :: iv
        integer(int64), intent(in) :: z
        integer(int64), intent(in) :: starts(:), ends(:)
        type(value_type), intent(in) :: plists(:)
        integer(int64), intent(in) :: position, length
        logical, intent(in) :: inherit
        integer(int64) :: k, a, b
        type(value_type) :: none

        if (length <= 0) return

        ! If the new text has no properties, then with inheritance it
        ! becomes part of whatever interval it was inserted into.  To
        ! prevent inheritance, we must clear out the properties of the
        ! newly inserted text.
        if (size(starts) == 0) then
            if (.not. inherit .and. iv%n > 0) call set_range_plist(iv, position, position + length, none, .false.)
            return
        end if

        ! Create an interval tree in which to place a copy of the intervals
        ! of the inserted string.
        if (iv%n == 0) call create_root_interval(iv, 1_int64, z - 1)
        ! The text without properties of its own.
        if (.not. inherit) call set_range_plist(iv, position, position + length, none, .false.)

        do k = 1, size(starts)
            a = position + max(0_int64, starts(k))
            b = position + min(length, ends(k))
            if (b <= a) cycle
            call set_range_plist(iv, a, b, plists(k), inherit)
        end do
    end subroutine graft_intervals_into_buffer

    !> Give the characters A..B of IV the properties PLIST (MERGE: add
    !> those of PLIST they lack instead), splitting intervals at A and B.
    !> Not a port.
    subroutine set_range_plist(iv, a, b, plist, merge)
        type(intervals_type), intent(inout) :: iv
        integer(int64), intent(in) :: a, b
        type(value_type), intent(in) :: plist
        logical, intent(in) :: merge
        integer(int64) :: k, r

        k = find_interval(iv, a)

        if (a > iv%runs(k)%position) then
            call split_interval_right(iv, k, a - iv%runs(k)%position, r)
            k = r
        end if

        do while (k <= iv%n)
            if (iv%runs(k)%position >= b) exit

            if (interval_end(iv, k) > b) then
                call split_interval_left(iv, k, b - iv%runs(k)%position, r)
                k = r
            end if

            if (merge) then
                call merge_properties(plist, iv%runs(k)%plist)
            else
                iv%runs(k)%plist = plist
            end if

            k = k + 1
        end do
    end subroutine set_range_plist

    !> The text properties of the LENGTH characters of IV from START, as
    !> pieces [STARTS(k), ENDS(k)) relative to START with properties
    !> PLISTS(k), the pieces without properties left out (none if IV has
    !> no intervals).  Port of intervals.c:copy_intervals (the copy as
    !> string intervals hold them).
    subroutine copy_intervals(iv, start, length, starts, ends, plists)
        type(intervals_type), intent(in) :: iv
        integer(int64), intent(in) :: start, length
        integer(int64), allocatable, intent(out) :: starts(:), ends(:)
        type(value_type), allocatable, intent(out) :: plists(:)
        integer(int64) :: k, n, a, b

        if (iv%n == 0 .or. length <= 0) then
            allocate (starts(0), ends(0), plists(0))
            return
        end if

        allocate (starts(iv%n), ends(iv%n), plists(iv%n))
        n = 0

        do k = find_interval(iv, start), iv%n
            if (iv%runs(k)%position >= start + length) exit
            a = max(start, iv%runs(k)%position)
            b = min(start + length, interval_end(iv, k))
            if (b <= a .or. .not. allocated(iv%runs(k)%plist%a)) cycle
            n = n + 1
            starts(n) = a - start
            ends(n) = b - start
            plists(n) = iv%runs(k)%plist
        end do

        call shrink(n)

    contains

        subroutine shrink(m)
            integer(int64), intent(in) :: m
            integer(int64), allocatable :: s(:), e(:)
            type(value_type), allocatable :: p(:)

            allocate (s(m), e(m), p(m))
            s = starts(1:m)
            e = ends(1:m)

            do a = 1, m
                p(a) = plists(a)
            end do

            call move_alloc(s, starts)
            call move_alloc(e, ends)
            call move_alloc(p, plists)
        end subroutine shrink
    end subroutine copy_intervals

    !> Give STRING the text properties of the LENGTH characters of the
    !> text with intervals IV from POSITION (none if it has none there).
    !> Port of intervals.c:copy_intervals_to_string.
    subroutine copy_intervals_to_string(string, iv, position, length)
        type(value_type), intent(inout) :: string
        type(intervals_type), intent(in) :: iv
        integer(int64), intent(in) :: position, length
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)

        call copy_intervals(iv, position, length, starts, ends, plists)
        if (size(starts) == 0) return
        call set_string_intervals(string, starts, ends, plists)
    end subroutine copy_intervals_to_string

    !> Whether the strings S1 and S2 have the same text properties
    !> everywhere (each property `eq').  Port of
    !> intervals.c:compare_string_intervals.
    logical function compare_string_intervals(s1, s2, nchars) result(same)
        type(value_type), intent(in) :: s1, s2
        integer(int64), intent(in) :: nchars
        integer(int64), allocatable :: st1(:), en1(:), st2(:), en2(:)
        type(value_type), allocatable :: pl1(:), pl2(:)
        integer(int64) :: pos

        call string_intervals(s1, st1, en1, pl1)
        call string_intervals(s2, st2, en2, pl2)
        same = .true.
        pos = 0

        ! Compare the plists in force at each position (both strings are
        ! NCHARS long), a run at a time.
        do while (pos < nchars)
            if (.not. intervals_equal(plist_at(st1, en1, pl1, pos), plist_at(st2, en2, pl2, pos))) then
                same = .false.
                return
            end if

            pos = min(next_bound(st1, en1, pos), next_bound(st2, en2, pos), nchars)
        end do

    contains

        function plist_at(st, en, pl, p) result(v)
            integer(int64), intent(in) :: st(:), en(:), p
            type(value_type), intent(in) :: pl(:)
            type(value_type) :: v
            integer :: i

            v = value_type()

            do i = 1, size(st)
                if (st(i) <= p .and. p < en(i)) then
                    v = pl(i)
                    return
                end if
            end do
        end function plist_at

        integer(int64) function next_bound(st, en, p) result(nb)
            integer(int64), intent(in) :: st(:), en(:), p
            integer :: i

            nb = huge(nb)

            do i = 1, size(st)
                if (st(i) > p) nb = min(nb, st(i))
                if (en(i) > p) nb = min(nb, en(i))
            end do
        end function next_bound
    end function compare_string_intervals
end module intervals_fm
