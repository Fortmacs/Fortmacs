! itree_fm.f90 -- overlays: the intervals they cover, and buffer.c's overlay functions.
! Ported from GNU Emacs 31.1 src/itree.c and src/buffer.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Overlays.  An overlay is a registry object referenced by a handle: the
!> buffer it is in (the null handle once deleted), the interval it
!> covers (itree.c's struct itree_node: begin, end, front_advance,
!> rear_advance) and its text properties.  Upstream keeps a buffer's
!> overlays in an interval tree (itree.c); here they are one array of
!> nodes, scanned in order of their start, which gives the same results
!> for the few overlays a buffer has.  insdel_fm moves them with the text
!> (`adjust_overlays_for_insert' and `adjust_overlays_for_delete', with
!> itree.c's insert_gap and delete_gap rules), kill-buffer deletes them,
!> and redisplay merges their `face' properties (xfaces_fm).
!>
!> No garbage collector frees overlays: `free_overlay' deletes an overlay
!> and releases its handle.
!>
!> Redisplay (xdisp_fm) also shows their `invisible', `display',
!> `before_string' and `after_string' properties, and textprop_fm's
!> `get_char_property' family looks at them.
!>
!> Modification hooks: insdel_fm's change notifications call
!> `report_overlay_modification', which runs the functions of the
!> `modification_hooks', `insert_in_front_hooks' and `insert_behind_hooks'
!> properties; a function there is a name of a defined
!> `property_function_type' (eval_fm).
!>
!> Deferred: indirect buffers (`copy_overlays'), the `window', `mouse_face' and
!> `intangible' properties, priorities given as two values, PRIMARY and SECONDARY
!> (no entry of two names), `remove_overlays' (subr.el) and
!> `copy_overlay'.
module itree_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: vtag, V_NULL, V_INTEGER, V_ARRAY, value_type
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use lisp_fm, only: value_type, xfixnum, make_list, list_elements
    use eval_fm, only: execute_overlay_change_function
    use data_fm, only: value_eq
    use buffer_fm, only: buffers, cur, is_buffer_live, buffer_handle, resolve_buffer, modiff_incr, &
                         buf_compute_unchanged, BUF_BEG => BEG
    use intervals_fm, only: textget
    implicit none(type, external)
    private

    public :: make_overlay, move_overlay, delete_overlay, free_overlay, delete_all_overlays, is_overlay, &
              overlay_start, overlay_end, overlay_buffer, overlay_get, overlay_get_prop, overlay_advance, overlay_put, &
              overlay_properties, &
              overlays_at, overlays_in, next_overlay_change, previous_overlay_change, sort_overlays, &
              adjust_overlays_for_insert, adjust_overlays_for_delete, buffer_has_overlays, overlay_priority, &
              compare_overlays, overlay_lists, overlay_recenter, is_overlay_touches, mouse_face_overlay_overlaps, &
              disable_line_numbers_overlay_at_eob, report_overlay_modification

    !> An overlay: struct Lisp_Overlay with its itree_node.
    type :: overlay_type
        !> The buffer the overlay is in; the null handle if deleted.
        type(handle_type) :: buffer
        integer(int64) :: begin = 0, end = 0
        logical :: front_advance = .false., rear_advance = .false.
        !> The overlay's properties.
        type(value_type) :: plist
    end type overlay_type

    type(slot_table_type) :: table
    type(overlay_type), allocatable :: overlays(:)

    !> The functions to run after a change and their overlays, recorded
    !> before it (buffer.c last_overlay_modification_hooks).
    type(value_type), allocatable :: last_hook_lists(:)
    type(handle_type), allocatable :: last_hook_overlays(:)
    integer :: last_hooks_used = 0

contains

    ! ------------------------------------------------------------------
    ! Objects
    ! ------------------------------------------------------------------

    !> A new overlay, in no buffer (buffer.c:build_overlay).  Port of
    !> itree.c:itree_node_init (the advance flags) and
    !> alloc.c:build_overlay.
    subroutine build_overlay(ov, front_advance, rear_advance)
        type(handle_type), intent(out) :: ov
        logical, intent(in) :: front_advance, rear_advance
        type(overlay_type), allocatable :: grown(:)

        call table%acquire(ov)

        if (.not. allocated(overlays)) then
            allocate (overlays(table%capacity()))
        else if (size(overlays, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(overlays)) = overlays
            call move_alloc(grown, overlays)
        end if

        overlays(ov%index) = overlay_type()
        overlays(ov%index)%front_advance = front_advance
        overlays(ov%index)%rear_advance = rear_advance
    end subroutine build_overlay

    !> Return true if OBJECT is an overlay.  Port of buffer.c:Foverlayp
    !> (a live overlay handle).
    logical function is_overlay(object)
        type(handle_type), intent(in) :: object

        is_overlay = table%is_live(object)
    end function is_overlay

    ! An error unless OV is an overlay.  Port of buffer.c:CHECK_OVERLAY.
    subroutine check_overlay(ov, status)
        type(handle_type), intent(in) :: ov
        type(status_type), intent(out) :: status

        if (.not. is_overlay(ov)) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(overlay_type)")
    end subroutine check_overlay

    !> Whether overlay slot I is in buffer slot B.  Not a port.
    logical function in_buffer(i, b)
        integer(int64), intent(in) :: i, b

        in_buffer = .false.
        if (b <= 0) return
        if (overlays(i)%buffer%index /= b) return
        in_buffer = is_buffer_live(overlays(i)%buffer)
    end function in_buffer

    !> The live overlay slots of buffer slot B, sorted by start (the order
    !> of an ascending ITREE_FOREACH).  Port of itree.c:itree_iterator_start,
    !> itree.c:itree_iterator_next, itree.c:itree_iterator_first_node and
    !> itree.c:itree_iter_next_in_subtree (in part: the nodes in order of
    !> their start, found by a scan and sorted, without a tree to walk).
    subroutine buffer_nodes(b, idx)
        integer(int64), intent(in) :: b
        integer(int64), allocatable, intent(out) :: idx(:)
        integer(int64) :: i, n, k, tmp

        if (.not. allocated(overlays)) then
            allocate (idx(0))
            return
        end if

        n = 0
        allocate (idx(size(overlays)))

        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, b)) cycle
            n = n + 1
            idx(n) = i
        end do

        call shrink_indices(idx, int(n))

        ! Insertion sort by begin (stable).
        do i = 2, n
            tmp = idx(i)
            k = i - 1

            do while (k >= 1)
                if (overlays(idx(k))%begin <= overlays(tmp)%begin) exit
                idx(k + 1) = idx(k)
                k = k - 1
            end do

            idx(k + 1) = tmp
        end do
    end subroutine buffer_nodes

    !> Keep the first N elements of IDX (into a new array: no reallocation
    !> to size 0, which valgrind reports).  Not a port.
    subroutine shrink_indices(idx, n)
        integer(int64), allocatable, intent(inout) :: idx(:)
        integer, intent(in) :: n
        integer(int64), allocatable :: kept(:)

        allocate (kept(n))
        kept = idx(1:n)
        call move_alloc(kept, idx)
    end subroutine shrink_indices

    !> Keep the first N elements of VEC.  Not a port.
    subroutine shrink_handles(vec, n)
        type(handle_type), allocatable, intent(inout) :: vec(:)
        integer, intent(in) :: n
        type(handle_type), allocatable :: kept(:)

        allocate (kept(n))
        kept = vec(1:n)
        call move_alloc(kept, vec)
    end subroutine shrink_handles

    !> Whether the current buffer has overlays (buffer.c:buffer_has_overlays).
    !> Port of buffer.h:buffer_has_overlays.
    logical function buffer_has_overlays()
        integer(int64) :: i

        buffer_has_overlays = .false.
        if (.not. allocated(overlays) .or. cur == 0) return

        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle

            if (in_buffer(i, cur)) then
                buffer_has_overlays = .true.
                return
            end if
        end do
    end function buffer_has_overlays

    !> Mark a section of buffer slot B as needing redisplay because of
    !> overlays changes.  Port of buffer.c:modify_overlay.
    subroutine modify_overlay(b, start_in, end_in)
        integer(int64), intent(in) :: b, start_in, end_in
        integer(int64) :: start, end

        start = min(start_in, end_in)
        end = max(start_in, end_in)
        call buf_compute_unchanged(buffers(b), start, end)
        buffers(b)%text%redisplay = .true.
        call modiff_incr(buffers(b)%text%overlay_modiff, 1_int64)
    end subroutine modify_overlay

    !> Create a new overlay with range BEG to END in BUFFER and return it.
    !> If omitted, BUFFER defaults to the current buffer.
    !> BEG and END may be integers or markers.
    !> The fourth arg FRONT_ADVANCE, if true, makes the marker
    !> for the front of the overlay advance when text is inserted there
    !> (which means the text *is not* included in the overlay).
    !> The fifth arg REAR_ADVANCE, if true, makes the marker
    !> for the rear of the overlay advance when text is inserted there
    !> (which means the text *is* included in the overlay).  Port of
    !> buffer.c:Fmake_overlay (BEG and END are positions).
    subroutine make_overlay(ov, beg, end, status, buffer, front_advance, rear_advance)
        type(handle_type), intent(out) :: ov
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: front_advance, rear_advance
        integer(int64) :: b, obeg, oend, lo, hi
        logical :: fa, ra

        ov = null_handle()
        b = cur

        if (present(buffer)) then
            if (buffer%index /= 0) then
                if (.not. is_buffer_live(buffer)) then
                    call set_error(status, FM_GENERIC_ERROR, "Attempt to create overlay in a dead buffer")
                    return
                end if

                b = buffer%index
            end if
        end if

        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to create overlay in a dead buffer")
            return
        end if

        fa = .false.
        if (present(front_advance)) fa = front_advance
        ra = .false.
        if (present(rear_advance)) ra = rear_advance
        lo = min(beg, end)
        hi = max(beg, end)
        obeg = clip(BUF_BEG, lo, buffers(b)%text%z)
        oend = clip(obeg, hi, buffers(b)%text%z)
        call build_overlay(ov, fa, ra)
        call add_buffer_overlay(ov%index, b, obeg, oend)
        ! We don't need to redisplay the region covered by the overlay,
        ! because the overlay has no properties at the moment.
    end subroutine make_overlay

    ! X clipped to [LO, HI].  Not a port.
    pure integer(int64) function clip(lo, x, hi)
        integer(int64), intent(in) :: lo, x, hi

        clip = max(lo, min(x, hi))
    end function clip

    !> Set the endpoints of OVERLAY to BEG and END in BUFFER.
    !> If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
    !> If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
    !> buffer.  Port of buffer.c:Fmove_overlay.
    subroutine move_overlay(overlay, beg, end, status, buffer)
        type(handle_type), intent(in) :: overlay
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: b, ob, n_beg, n_end, o_beg, o_end, lo, hi, i

        call check_overlay(overlay, status)
        if (.not. status%ok()) return
        i = overlay%index
        b = 0
        if (present(buffer)) b = buffer%index

        if (b == 0) then
            if (is_buffer_live(overlays(i)%buffer)) b = overlays(i)%buffer%index
        end if

        if (b == 0) b = cur

        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to move overlay to a dead buffer")
            return
        end if

        if (present(buffer)) then
            if (buffer%index /= 0 .and. .not. is_buffer_live(buffer)) then
                call set_error(status, FM_GENERIC_ERROR, "Attempt to move overlay to a dead buffer")
                return
            end if
        end if

        lo = min(beg, end)
        hi = max(beg, end)
        ob = 0
        if (is_buffer_live(overlays(i)%buffer)) ob = overlays(i)%buffer%index
        n_beg = clip(BUF_BEG, lo, buffers(b)%text%z)
        n_end = clip(n_beg, hi, buffers(b)%text%z)
        o_beg = 0
        o_end = 0

        if (ob /= 0) then
            o_beg = overlays(i)%begin
            o_end = overlays(i)%end
        end if

        if (b /= ob) then
            if (ob /= 0) call remove_buffer_overlay(i)
            call add_buffer_overlay(i, b, n_beg, n_end)
        else
            call itree_node_set_region(i, n_beg, n_end)
        end if

        if (b /= ob) then
            ! If the overlay has changed buffers, do a thorough redisplay.
            ! Redisplay where the overlay was.
            if (ob /= 0) call modify_overlay(ob, o_beg, o_end)
            ! Redisplay where the overlay is going to be.
            call modify_overlay(b, n_beg, n_end)
        else
            ! Redisplay the area the overlay has just left, or just
            ! enclosed.
            if (o_beg == n_beg) then
                call modify_overlay(b, o_end, n_end)
            else if (o_end == n_end) then
                call modify_overlay(b, o_beg, n_beg)
            else
                call modify_overlay(b, min(o_beg, n_beg), max(o_end, n_end))
            end if
        end if

        ! Delete the overlay if it is empty after clipping and has the
        ! evaporate property.
        if (n_beg == n_end) then
            if (vtag(overlay_get(overlay, "evaporate")) /= V_NULL) call drop_overlay(i)
        end if
    end subroutine move_overlay

    !> Put overlay slot I into buffer slot B, covering BEGIN to END.
    !> Port of buffer.c:add_buffer_overlay with itree.c:itree_insert and
    !> itree.c:itree_insert_node (in part:
    !> joining the buffer's nodes is setting the buffer; there is no tree
    !> to rebalance).
    subroutine add_buffer_overlay(i, b, begin, end)
        integer(int64), intent(in) :: i, b, begin, end

        overlays(i)%buffer = buffer_handle(b)
        overlays(i)%begin = begin
        overlays(i)%end = end
    end subroutine add_buffer_overlay

    !> Take overlay slot I out of its buffer.  Port of
    !> buffer.c:remove_buffer_overlay with itree.c:itree_remove (in part:
    !> no tree to rebalance).
    subroutine remove_buffer_overlay(i)
        integer(int64), intent(in) :: i

        overlays(i)%buffer = null_handle()
    end subroutine remove_buffer_overlay

    !> Give overlay slot I, which stays in its buffer, the bounds BEGIN and
    !> END.  Port of itree.c:itree_node_set_region (no node to move in a
    !> tree).
    subroutine itree_node_set_region(i, begin, end)
        integer(int64), intent(in) :: i, begin, end

        overlays(i)%begin = begin
        overlays(i)%end = end
    end subroutine itree_node_set_region

    !> Remove overlay slot I from its buffer (buffer.c:drop_overlay).
    !> Port of buffer.c:drop_overlay.
    subroutine drop_overlay(i)
        integer(int64), intent(in) :: i

        if (is_buffer_live(overlays(i)%buffer)) then
            call modify_overlay(overlays(i)%buffer%index, overlays(i)%begin, overlays(i)%end)
        end if

        call remove_buffer_overlay(i)
    end subroutine drop_overlay

    !> Delete the overlay OVERLAY from its buffer.  Port of
    !> buffer.c:Fdelete_overlay.
    subroutine delete_overlay(overlay)
        type(handle_type), intent(in) :: overlay

        if (.not. is_overlay(overlay)) return
        if (.not. is_buffer_live(overlays(overlay%index)%buffer)) return
        call drop_overlay(overlay%index)
    end subroutine delete_overlay

    !> Delete OVERLAY and release its handle, which becomes stale.  Not a port:
    !> upstream relies on GC.
    subroutine free_overlay(overlay)
        type(handle_type), intent(in) :: overlay
        type(status_type) :: st

        if (.not. is_overlay(overlay)) return
        call delete_overlay(overlay)
        overlays(overlay%index) = overlay_type()
        call table%release(overlay, st)
    end subroutine free_overlay

    !> Delete all overlays of BUFFER.
    !> BUFFER omitted means delete all overlays of the current
    !> buffer.  Port of buffer.c:Fdelete_all_overlays (with
    !> buffer.c:delete_all_overlays and itree.c:itree_clear).
    subroutine delete_all_overlays(buffer)
        type(handle_type), intent(in), optional :: buffer
        integer(int64), allocatable :: idx(:)
        integer(int64) :: b
        integer :: k

        b = cur

        if (present(buffer)) then
            b = 0
            if (is_buffer_live(buffer)) b = buffer%index
        end if

        if (b == 0) return
        call buffer_nodes(b, idx)

        do k = 1, size(idx)
            call drop_overlay(idx(k))
        end do
    end subroutine delete_all_overlays

    !> Return the position at which OVERLAY starts (0 if it is deleted).
    !> Port of buffer.c:Foverlay_start (with
    !> itree.c:itree_node_begin: no offsets to add up).
    integer(int64) function overlay_start(overlay)
        type(handle_type), intent(in) :: overlay

        overlay_start = 0
        if (.not. is_overlay(overlay)) return
        if (is_buffer_live(overlays(overlay%index)%buffer)) overlay_start = overlays(overlay%index)%begin
    end function overlay_start

    !> Return the position at which OVERLAY ends (0 if it is deleted).
    !> Port of buffer.c:Foverlay_end (with
    !> itree.c:itree_node_end).
    integer(int64) function overlay_end(overlay)
        type(handle_type), intent(in) :: overlay

        overlay_end = 0
        if (.not. is_overlay(overlay)) return
        if (is_buffer_live(overlays(overlay%index)%buffer)) overlay_end = overlays(overlay%index)%end
    end function overlay_end

    !> Return the buffer OVERLAY belongs to.
    !> Return the null handle if OVERLAY has been deleted.  Port of
    !> buffer.c:Foverlay_buffer.
    function overlay_buffer(overlay) result(b)
        type(handle_type), intent(in) :: overlay
        type(handle_type) :: b

        b = null_handle()
        if (.not. is_overlay(overlay)) return
        if (is_buffer_live(overlays(overlay%index)%buffer)) b = overlays(overlay%index)%buffer
    end function overlay_buffer

    !> Return a list of the properties on OVERLAY.
    !> This is a copy of OVERLAY's properties; modifying it has no effect on
    !> OVERLAY.  Port of buffer.c:Foverlay_properties.
    function overlay_properties(overlay) result(plist)
        type(handle_type), intent(in) :: overlay
        type(value_type) :: plist

        if (is_overlay(overlay)) plist = overlays(overlay%index)%plist
    end function overlay_properties

    !> Get the property of overlay OVERLAY with property name PROP.  Port of
    !> buffer.c:Foverlay_get.
    function overlay_get(overlay, prop) result(val)
        type(handle_type), intent(in) :: overlay
        character(len=*), intent(in) :: prop
        type(value_type) :: val

        if (.not. is_overlay(overlay)) return
        val = textget(overlays(overlay%index)%plist, value_type(name=prop))
    end function overlay_get

    !> Whether text inserted at OVERLAY's start goes outside it
    !> (front-advance), and at its end inside (rear-advance).  Port of
    !> buffer.h:OVERLAY_FRONT_ADVANCE_P and OVERLAY_REAR_ADVANCE_P.
    subroutine overlay_advance(overlay, front_advance, rear_advance)
        type(handle_type), intent(in) :: overlay
        logical, intent(out) :: front_advance, rear_advance

        front_advance = .false.
        rear_advance = .false.
        if (.not. is_overlay(overlay)) return
        front_advance = overlays(overlay%index)%front_advance
        rear_advance = overlays(overlay%index)%rear_advance
    end subroutine overlay_advance

    !> `overlay_get' with the property given as a name value.  Port of
    !> buffer.c:Foverlay_get.
    function overlay_get_prop(overlay, prop) result(val)
        type(handle_type), intent(in) :: overlay
        type(value_type), intent(in) :: prop
        type(value_type) :: val

        if (.not. is_overlay(overlay)) return
        val = textget(overlays(overlay%index)%plist, prop)
    end function overlay_get_prop

    !> Set one property of overlay OVERLAY: give property PROP value VALUE.
    !> Port of buffer.c:Foverlay_put.
    subroutine overlay_put(overlay, prop, value, status)
        type(handle_type), intent(in) :: overlay
        character(len=*), intent(in) :: prop
        type(value_type), intent(in) :: value
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:), grown(:)
        type(value_type) :: key
        integer :: k, n
        logical :: changed, found
        integer(int64) :: i

        call check_overlay(overlay, status)
        if (.not. status%ok()) return
        i = overlay%index
        key = value_type(name=prop)
        call list_elements(overlays(i)%plist, elems)
        found = .false.
        changed = .false.

        do k = 1, size(elems) - 1, 2
            if (value_eq(elems(k), key)) then
                changed = .not. value_eq(elems(k + 1), value)
                elems(k + 1) = value
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            ! It wasn't in the list, so add it to the front.
            changed = allocated(value%a)
            n = size(elems)
            allocate (grown(n + 2))
            grown(1) = key
            grown(2) = value
            grown(3:n + 2) = elems
            call move_alloc(grown, elems)
        end if

        overlays(i)%plist = make_list(elems)

        if (is_buffer_live(overlays(i)%buffer)) then
            if (changed) call modify_overlay(overlays(i)%buffer%index, overlays(i)%begin, overlays(i)%end)

            if (prop == "evaporate" .and. allocated(value%a) .and. overlays(i)%begin == overlays(i)%end) then
                call delete_overlay(overlay)
            end if
        end if
    end subroutine overlay_put

    ! ------------------------------------------------------------------
    ! Finding overlays
    ! ------------------------------------------------------------------

    !> Find all the overlays in the current buffer that overlap the range
    !> BEG-END, or are empty at BEG, or are empty at END provided END
    !> denotes the position at the end of the accessible part of the buffer.
    !>
    !> Return them in VEC, and in NEXT the next position after END where
    !> an overlay starts, or Z if there are no more overlays after.
    !> EMPTY: include empty overlays.  Port of buffer.c:overlays_in.
    subroutine overlays_in_1(beg, end, empty, trailing, vec, next)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: empty, trailing
        type(handle_type), allocatable, intent(out) :: vec(:)
        integer(int64), intent(out) :: next
        integer(int64), allocatable :: idx(:)
        integer(int64) :: z, search_end, nb, ne
        integer :: k, n

        z = buffers(cur)%text%z
        next = z
        call buffer_nodes(cur, idx)
        ! Extend the search range if overlays beginning at Z are wanted.
        search_end = z
        if (end >= z .and. (empty .or. trailing)) search_end = search_end + 1
        allocate (vec(size(idx)))
        n = 0

        do k = 1, size(idx)
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            ! ITREE_FOREACH (BEG, SEARCH_END): nodes intersecting it.
            if (.not. intersects(nb, ne, beg, search_end)) cycle

            if (nb > end) then
                next = min(next, nb)
                exit
            else if (nb == end) then
                next = nb
                if ((.not. empty .or. end < z) .and. beg < end) exit
                if (empty .and. nb /= ne) cycle
            end if

            if (.not. empty .and. nb == ne) cycle
            n = n + 1
            vec(n) = table%handle_of(idx(k))
        end do

        call shrink_handles(vec, n)
    end subroutine overlays_in_1

    !> Whether node [NB, NE] intersects the search range [B, E]
    !> (itree.c:itree_node_intersects).  Port of
    !> itree.c:itree_node_intersects.
    pure logical function intersects(nb, ne, b, e)
        integer(int64), intent(in) :: nb, ne, b, e

        intersects = (b < ne .and. nb < e) .or. (nb == ne .and. b == nb)
    end function intersects

    !> Return a list of the overlays that contain the character at POS.
    !> If SORTED is true, then sort them by decreasing priority.
    !>
    !> Zero-length (a.k.a. "empty") overlays that start and stop at POS are not
    !> included in the return value.  Use `overlays_in' if empty overlays are of
    !> interest.
    !>
    !> This function can return overlays outside of the current narrowing of
    !> the buffer if POS is outside of the narrowing.  Port of
    !> buffer.c:Foverlays_at (with buffer.c:overlays_at; NEXT, if present, is the
    !> next position after POS where an overlay starts).
    subroutine overlays_at(pos, vec, sorted, next)
        integer(int64), intent(in) :: pos
        type(handle_type), allocatable, intent(out) :: vec(:)
        logical, intent(in), optional :: sorted
        integer(int64), intent(out), optional :: next
        integer(int64) :: nxt
        integer :: k, n
        type(handle_type) :: tmp

        if (cur == 0) then
            allocate (vec(0))
            if (present(next)) next = 0
            return
        end if

        call overlays_in_1(pos, pos + 1, .false., .true., vec, nxt)
        if (present(next)) next = nxt
        if (.not. present(sorted)) return
        if (.not. sorted) return
        call sort_overlays(vec)
        ! The list should be in decreasing order of priority.
        n = size(vec)

        do k = 1, n/2
            tmp = vec(k)
            vec(k) = vec(n + 1 - k)
            vec(n + 1 - k) = tmp
        end do
    end subroutine overlays_at

    !> Return a list of the overlays that overlap the region BEG ... END.
    !> Overlap means that at least one character between BEG and END is contained
    !> within the overlay.  Note that this excludes the character at (i.e., after)
    !> END.
    !>
    !> Empty overlays do not contain any characters, so they are included in the
    !> result if they are located at BEG, between BEG and END, or at END provided
    !> END denotes the position at the end of the buffer.  (If BEG and END are the
    !> same buffer position, an empty overlay at BEG will also be at END, but it
    !> still qualifies because it's at BEG.)
    !>
    !> The resulting list of overlays is in an arbitrary unpredictable order.
    !>
    !> This function can return overlays outside of the current narrowing of
    !> the buffer if BEG and/or END are outside of the narrowing.  Port of
    !> buffer.c:Foverlays_in.
    subroutine overlays_in(beg, end, vec)
        integer(int64), intent(in) :: beg, end
        type(handle_type), allocatable, intent(out) :: vec(:)
        integer(int64) :: next

        if (cur == 0) then
            allocate (vec(0))
            return
        end if

        call overlays_in_1(beg, end, .true., .false., vec, next)
    end subroutine overlays_in

    !> Return the next position after POS where an overlay starts or ends.
    !> If there are no overlay boundaries from POS to (point_max),
    !> the value is (point_max).  Port of buffer.c:Fnext_overlay_change
    !> (with buffer.c:next_overlay_change).
    integer(int64) function next_overlay_change(pos) result(next)
        integer(int64), intent(in) :: pos
        integer(int64), allocatable :: idx(:)
        integer(int64) :: nb, ne
        integer :: k

        next = 0
        if (cur == 0) return
        next = buffers(cur)%zv
        call buffer_nodes(cur, idx)

        do k = 1, size(idx)
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            if (.not. intersects(nb, ne, pos, next)) cycle

            if (nb > pos) then
                ! NB must be the least upper bound of POS, because the
                ! search is limited to [POS, NEXT).
                if (nb < next) next = nb
                exit
            else if (nb < ne .and. ne < next) then
                next = ne
            end if
        end do
    end function next_overlay_change

    !> Return the previous position before POS where an overlay starts or ends.
    !> If there are no overlay boundaries from (point_min) to POS,
    !> the value is (point_min).  Port of buffer.c:Fprevious_overlay_change
    !> (with buffer.c:previous_overlay_change).
    integer(int64) function previous_overlay_change(pos) result(prev)
        integer(int64), intent(in) :: pos
        integer(int64), allocatable :: idx(:)
        integer(int64) :: nb, ne
        integer :: k

        prev = 0
        if (cur == 0) return
        prev = buffers(cur)%begv
        call buffer_nodes(cur, idx)

        do k = size(idx), 1, -1
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            if (.not. intersects(nb, ne, prev, pos)) cycle

            if (ne < pos) then
                prev = max(prev, ne)
            else
                prev = max(prev, nb)
            end if
        end do
    end function previous_overlay_change

    !> The priority of OVERLAY in PRIORITY and its secondary priority in
    !> SPRIORITY: its `priority' property if a fixnum, else 0
    !> (buffer.c:make_sortvec_item).  Port of the priority part of
    !> buffer.c:make_sortvec_item.
    subroutine overlay_priority(overlay, priority, spriority)
        type(handle_type), intent(in) :: overlay
        integer(int64), intent(out) :: priority, spriority
        type(value_type) :: tem

        priority = 0
        spriority = 0
        tem = overlay_get(overlay, "priority")
        if (vtag(tem) == V_INTEGER) priority = xfixnum(tem)
    end subroutine overlay_priority

    !> Sort an array of overlays by priority, increasing.  Invalid
    !> (deleted) overlays are dropped.  Port of buffer.c:sort_overlays
    !> with compare_overlays (no window).
    subroutine sort_overlays(vec)
        type(handle_type), allocatable, intent(inout) :: vec(:)
        integer(int64), allocatable :: prio(:), sprio(:)
        type(handle_type) :: tmp
        integer(int64) :: tp, ts
        integer :: i, k, n

        ! Put the valid and relevant overlays first.
        n = 0

        do i = 1, size(vec)
            if (overlay_start(vec(i)) > 0 .and. overlay_end(vec(i)) > 0) then
                n = n + 1
                vec(n) = vec(i)
            end if
        end do

        call shrink_handles(vec, n)
        allocate (prio(n), sprio(n))

        do i = 1, n
            call overlay_priority(vec(i), prio(i), sprio(i))
        end do

        ! Sort the overlays into the proper order: increasing priority.
        do i = 2, n
            tmp = vec(i)
            tp = prio(i)
            ts = sprio(i)
            k = i - 1

            do while (k >= 1)
                if (compare_overlays(prio(k), sprio(k), overlay_start(vec(k)), overlay_end(vec(k)), vec(k)%index, &
                                     tp, ts, overlay_start(tmp), overlay_end(tmp), tmp%index) <= 0) exit
                vec(k + 1) = vec(k)
                prio(k + 1) = prio(k)
                sprio(k + 1) = sprio(k)
                k = k - 1
            end do

            vec(k + 1) = tmp
            prio(k + 1) = tp
            sprio(k + 1) = ts
        end do
    end subroutine sort_overlays

    !> 1 if the first overlay should take precedence, -1 if the second
    !> should, 0 if they are the same: overlays with priorities P1 and
    !> P2, secondary priorities S1 and S2, bounds B1-E1 and B2-E2 and
    !> tie-breaking ids K1 and K2 (the same id for the same overlay).
    !> Port of buffer.c:compare_overlays (on struct sortvec items).
    pure integer function compare_overlays(p1, s1, b1, e1, k1, p2, s2, b2, e2, k2) result(c)
        integer(int64), intent(in) :: p1, s1, b1, e1, k1, p2, s2, b2, e2, k2

        if (p1 /= p2) then
            c = merge(-1, 1, p1 < p2)
        else if (b1 < b2) then
            ! If the priority is equal, give precedence to the one not
            ! covered by the other.  If neither covers the other, obey
            ! spriority.
            c = merge(1, -1, e1 < e2 .and. s1 > s2)
        else if (b1 > b2) then
            c = merge(-1, 1, e1 > e2 .and. s1 < s2)
        else if (e1 /= e2) then
            c = merge(-1, 1, e2 < e1)
        else if (s1 /= s2) then
            c = merge(-1, 1, s1 < s2)
        else if (k1 == k2) then
            c = 0
        else
            ! Avoid the non-determinism of qsort by choosing an arbitrary
            ! ordering between "equal" overlays.
            c = merge(-1, 1, k1 < k2)
        end if
    end function compare_overlays

    ! ------------------------------------------------------------------
    ! Moving with the text
    ! ------------------------------------------------------------------

    !> Insert a gap at POS of length LENGTH expanding all intervals
    !> intersecting it, while respecting their rear_advance and
    !> front_advance setting.
    !>
    !> If BEFORE_MARKERS is true, all overlays beginning/ending at POS
    !> are treated as if their front_advance/rear_advance was true.  Port of
    !> buffer.c:adjust_overlays_for_insert with itree.c:itree_insert_gap
    !> (the current buffer).
    subroutine adjust_overlays_for_insert(pos, length, before_markers)
        integer(int64), intent(in) :: pos, length
        logical, intent(in) :: before_markers
        integer(int64) :: i

        if (length <= 0 .or. cur == 0 .or. .not. allocated(overlays)) return

        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle

            associate (node => overlays(i))
                if (.not. before_markers .and. node%begin == pos .and. node%front_advance .and. &
                    (node%begin /= node%end .or. node%rear_advance)) then
                    ! Nodes with front_advance starting at POS move as a
                    ! whole (removed and reinserted upstream).  If we have
                    ! front_advance and not rear_advance and the overlay is
                    ! empty, we don't move begin past end by pretending
                    ! it's not front_advance.
                    node%begin = node%begin + length
                    node%end = node%end + length
                else
                    if (before_markers) then
                        if (node%begin >= pos) node%begin = node%begin + length
                    else if (node%begin > pos) then
                        node%begin = node%begin + length
                    end if

                    if (node%end > pos) then
                        node%end = node%end + length
                    else if (node%end == pos .and. (before_markers .or. node%rear_advance)) then
                        node%end = node%end + length
                    end if
                end if
            end associate
        end do
    end subroutine adjust_overlays_for_insert

    !> Delete a gap at POS of length LENGTH, contracting all intervals
    !> intersecting it, then delete the empty overlays at POS that have
    !> the `evaporate' property.  Port of
    !> buffer.c:adjust_overlays_for_delete (buffer.c:adjust_overlays_for_delete_in_buffer)
    !> with itree.c:itree_delete_gap (the current buffer).
    subroutine adjust_overlays_for_delete(pos, length)
        integer(int64), intent(in) :: pos, length
        type(handle_type), allocatable :: hit_list(:)
        integer(int64) :: i
        integer :: n, k

        if (length <= 0 .or. cur == 0 .or. .not. allocated(overlays)) return

        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle

            associate (node => overlays(i))
                if (pos < node%begin) node%begin = max(pos, node%begin - length)
                if (node%end > pos) node%end = max(pos, node%end - length)
            end associate
        end do

        ! Delete any zero-sized overlays at position POS, if the
        ! `evaporate' property is set.
        allocate (hit_list(size(overlays)))
        n = 0

        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle
            if (overlays(i)%begin /= pos .or. overlays(i)%end /= pos) cycle
            if (vtag(overlay_get(table%handle_of(i), "evaporate")) == V_NULL) cycle
            n = n + 1
            hit_list(n) = table%handle_of(i)
        end do

        do k = 1, n
            call delete_overlay(hit_list(k))
        end do
    end subroutine adjust_overlays_for_delete

    ! ------------------------------------------------------------------
    ! Lists, boundaries and modification hooks
    ! ------------------------------------------------------------------

    !> Return a list giving all the overlays of the current buffer.
    !>
    !> For backward compatibility, the value is actually a list that
    !> holds another list; the overlays are in the inner list.
    !> The list you get is a copy, so that changing it has no effect.
    !> However, the overlays you get are the real objects that the buffer uses.
    !> Port of buffer.c:Foverlay_lists (VEC is the inner list, in order of
    !> increasing start; the outer list holds nothing else).
    subroutine overlay_lists(vec)
        type(handle_type), allocatable, intent(out) :: vec(:)
        integer(int64), allocatable :: idx(:)
        integer :: k

        if (cur == 0) then
            allocate (vec(0))
            return
        end if

        call buffer_nodes(cur, idx)
        allocate (vec(size(idx)))

        do k = 1, size(idx)
            vec(k) = table%handle_of(idx(k))
        end do
    end subroutine overlay_lists

    !> Recenter the overlays of the current buffer around position POS.
    !> That makes overlay lookup faster for positions near POS (but perhaps slower
    !> for positions far away from POS).
    !>
    !> Since Emacs 29.1, this function is a no-op, because the implementation
    !> of overlays changed and their lookup is now fast regardless of their
    !> position in the buffer.  In particular, this function no longer affects
    !> the value returned by `overlay_lists'.
    !> Port of buffer.c:Foverlay_recenter.
    subroutine overlay_recenter(pos)
        integer(int64), intent(in) :: pos

        ! Noop
        if (pos == 0) continue
    end subroutine overlay_recenter

    !> Whether some overlay of the current buffer starts or ends (or both)
    !> at POS.  Port of buffer.c:overlay_touches_p.
    logical function is_overlay_touches(pos)
        integer(int64), intent(in) :: pos
        integer(int64), allocatable :: idx(:)
        integer :: k

        is_overlay_touches = .false.
        if (cur == 0) return
        call buffer_nodes(cur, idx)

        ! We need to find overlays ending in pos, as well as empty ones at
        ! pos.
        do k = size(idx), 1, -1
            associate (node => overlays(idx(k)))
                if (.not. intersects(node%begin, node%end, pos - 1, pos + 1)) cycle

                if (node%begin == pos .or. node%end == pos) then
                    is_overlay_touches = .true.
                    return
                end if
            end associate
        end do
    end function is_overlay_touches

    !> Whether another non-empty overlay of the current buffer with a
    !> true `mouse_face' property overlaps OVERLAY.  Port of
    !> buffer.c:mouse_face_overlay_overlaps (which tests OVERLAY's own
    !> `mouse_face' property for each, as upstream does).
    logical function mouse_face_overlay_overlaps(overlay)
        type(handle_type), intent(in) :: overlay
        integer(int64), allocatable :: idx(:)
        integer(int64) :: start, end
        integer :: k

        mouse_face_overlay_overlaps = .false.
        if (cur == 0 .or. .not. is_overlay(overlay)) return
        start = overlays(overlay%index)%begin
        end = overlays(overlay%index)%end
        call buffer_nodes(cur, idx)

        do k = 1, size(idx)
            associate (node => overlays(idx(k)))
                if (.not. intersects(node%begin, node%end, start, min(end, buffers(cur)%zv) + 1)) cycle
                if (.not. (node%begin < end .and. node%end > start .and. node%begin < node%end)) cycle
                if (idx(k) == overlay%index) cycle

                if (vtag(overlay_get(overlay, "mouse_face")) /= V_NULL) then
                    mouse_face_overlay_overlaps = .true.
                    return
                end if
            end associate
        end do
    end function mouse_face_overlay_overlaps

    !> Whether an overlay at the end of the accessible text has a true
    !> `display_line_numbers_disable' property.  Port of
    !> buffer.c:disable_line_numbers_overlay_at_eob.
    logical function disable_line_numbers_overlay_at_eob()
        integer(int64), allocatable :: idx(:)
        integer(int64) :: zv
        integer :: k

        disable_line_numbers_overlay_at_eob = .false.
        if (cur == 0) return
        zv = buffers(cur)%zv
        call buffer_nodes(cur, idx)

        do k = 1, size(idx)
            if (.not. intersects(overlays(idx(k))%begin, overlays(idx(k))%end, zv, zv)) cycle

            if (vtag(overlay_get(table%handle_of(idx(k)), "display_line_numbers_disable")) /= V_NULL) then
                disable_line_numbers_overlay_at_eob = .true.
                return
            end if
        end do
    end function disable_line_numbers_overlay_at_eob

    !> Record the functions FUNCTIONLIST of OVERLAY to run after the change.
    !> Port of buffer.c:add_overlay_mod_hooklist.
    subroutine add_overlay_mod_hooklist(functionlist, overlay)
        type(value_type), intent(in) :: functionlist
        type(handle_type), intent(in) :: overlay
        type(value_type), allocatable :: grown(:)
        type(handle_type), allocatable :: grown_overlays(:)
        integer :: k

        if (.not. allocated(last_hook_lists)) then
            allocate (last_hook_lists(16), last_hook_overlays(16))
        else if (last_hooks_used == size(last_hook_lists)) then
            allocate (grown(2*last_hooks_used), grown_overlays(2*last_hooks_used))

            do k = 1, last_hooks_used
                grown(k) = last_hook_lists(k)
                grown_overlays(k) = last_hook_overlays(k)
            end do

            call move_alloc(grown, last_hook_lists)
            call move_alloc(grown_overlays, last_hook_overlays)
        end if

        last_hooks_used = last_hooks_used + 1
        last_hook_lists(last_hooks_used) = functionlist
        last_hook_overlays(last_hooks_used) = overlay
    end subroutine add_overlay_mod_hooklist

    !> Run the modification-hooks of overlays that include any part of the
    !> text in START to END.  If this change is an insertion, also run the
    !> insert-in-front-hooks of overlays starting at END, and the
    !> insert-behind-hooks of overlays ending at START.
    !>
    !> This is called both before and after the modification.  AFTER is true
    !> when we call after the modification; then the functions recorded
    !> before it run again.
    !>
    !> ARG1, ARG2, ARG3 are arguments to pass to the hook functions.  When
    !> AFTER is true, they are the start position, the position after the
    !> inserted new text, and the length of deleted or replaced old text
    !> (ARG3 is absent before the change).  The first function to fail stops
    !> them and its error is returned.  Port of
    !> buffer.c:report_overlay_modification.
    recursive subroutine report_overlay_modification(start, end, after, arg1, arg2, status, arg3)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: after
        integer(int64), intent(in) :: arg1, arg2
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg3
        type(value_type), allocatable :: copy(:)
        type(handle_type), allocatable :: copy_overlays(:)
        integer(int64), allocatable :: idx(:)
        integer(int64) :: obegin, oend, b, e
        type(value_type) :: prop
        type(handle_type) :: ov
        logical :: insertion
        integer :: k, size_used

        status = status_type()

        ! True if this change is an insertion.
        if (after) then
            insertion = .false.
            if (present(arg3)) insertion = arg3 == 0
        else
            insertion = start == end
        end if

        if (.not. after) then
            ! We are being called before a change.  Scan the overlays to
            ! find the functions to call.
            last_hooks_used = 0
            if (cur == 0 .or. .not. allocated(overlays)) return
            b = start
            e = end

            if (insertion) then
                b = b - 1
                e = e + 1
            end if

            call buffer_nodes(cur, idx)

            do k = 1, size(idx)
                if (.not. intersects(overlays(idx(k))%begin, overlays(idx(k))%end, b, e)) cycle
                ov = table%handle_of(idx(k))
                obegin = overlays(idx(k))%begin
                oend = overlays(idx(k))%end

                if (insertion .and. (start == obegin .or. end == obegin)) then
                    prop = overlay_get(ov, "insert_in_front_hooks")
                    if (allocated(prop%a)) call add_overlay_mod_hooklist(prop, ov)
                end if

                if (insertion .and. (start == oend .or. end == oend)) then
                    prop = overlay_get(ov, "insert_behind_hooks")
                    if (allocated(prop%a)) call add_overlay_mod_hooklist(prop, ov)
                end if

                ! Test for intersecting intervals.  This does the right
                ! thing for both insertion and deletion.
                if (end > obegin .and. start < oend) then
                    prop = overlay_get(ov, "modification_hooks")
                    if (allocated(prop%a)) call add_overlay_mod_hooklist(prop, ov)
                end if
            end do
        end if

        ! Call the functions recorded in last_overlay_modification_hooks.
        ! First copy the vector contents, in case some of these hooks do
        ! subsequent modification of the buffer.
        size_used = last_hooks_used
        allocate (copy(size_used), copy_overlays(size_used))

        do k = 1, size_used
            copy(k) = last_hook_lists(k)
            copy_overlays(k) = last_hook_overlays(k)
        end do

        do k = 1, size_used
            ! It is possible that the recorded overlay has been deleted
            ! (which makes its markers' buffers be nil), or that (due to
            ! some bug) it belongs to a different buffer.  Only run this
            ! hook if the overlay belongs to the current buffer.
            if (.not. is_overlay(copy_overlays(k))) cycle
            if (.not. is_buffer_live(overlays(copy_overlays(k)%index)%buffer)) cycle
            if (overlays(copy_overlays(k)%index)%buffer%index /= cur) cycle
            call execute_overlay_mod_hooks(copy(k), copy_overlays(k), after, arg1, arg2, status, arg3)
            if (.not. status%ok()) return
        end do
    end subroutine report_overlay_modification

    !> Call each function of LIST with OVERLAY, AFTER, ARG1, ARG2 and, if
    !> present, ARG3.  Port of buffer.c:call_overlay_mod_hooks.
    recursive subroutine execute_overlay_mod_hooks(list, overlay, after, arg1, arg2, status, arg3)
        type(value_type), intent(in) :: list
        type(handle_type), intent(in) :: overlay
        logical, intent(in) :: after
        integer(int64), intent(in) :: arg1, arg2
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg3
        type(value_type), allocatable :: fns(:)
        integer :: k

        status = status_type()
        if (vtag(list) /= V_ARRAY) return
        call list_elements(list, fns)

        do k = 1, size(fns)
            call execute_overlay_change_function(fns(k), overlay, after, arg1, arg2, status, arg3)
            if (.not. status%ok()) return
        end do
    end subroutine execute_overlay_mod_hooks
end module itree_fm
