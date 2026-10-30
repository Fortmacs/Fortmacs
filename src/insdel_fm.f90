! insdel_fm.f90 -- buffer insertion, deletion and gap motion.
! Ported from GNU Emacs 31.1 src/insdel.c.
!
! Copyright (C) 1985-1986, 1993-1995, 1997-2026 Free Software
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

!> Inserting and deleting text in the current buffer, and the change
!> hooks.
!>
!> All operations act on the current buffer, as upstream.  Text to insert
!> is given in the buffer's own representation (multibyte text for a
!> multibyte buffer); `insert_char' encodes a single character.
!>
!> Every change runs `before_change_functions' (and `first_change_hook'
!> on the first change of an unmodified buffer) before it and
!> `after_change_functions' after it, unless `inhibit_modification_hooks'
!> is true, which it is while they run.  The primitives that leave this
!> to their callers say so (`insert_1_both', `del_range_2',
!> `replace_range_2', `modify_text').
!>
!> Overlays move with the text (itree_fm).
!>
!> Deferred until their subsystems exist: overlay modification hooks, text property modification hooks (`modification_hooks',
!> `insert_in_front_hooks', `insert_behind_hooks'),
!> `combine_after_change_calls', file locking and `select_active_regions'
!> in `prepare_to_modify_buffer', invalidating the region caches (there
!> are none yet), inheriting text properties, compositions, tree-sitter,
!> quitting in the middle of a gap move, `copy_text' conversions,
!> `replace_range' with a buffer as the new text, and
!> `insert_from_string/buffer' (strings are inserted with `insert').
module insdel_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_MEMORY_FULL
    use character_fm, only: char_string, multibyte_chars_in_text, string_char_and_length, str_to_multibyte, &
                            MAX_MULTIBYTE_LENGTH
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, BEG, BUF_BYTES_MAX, GAP_BYTES_DFL, &
                         GAP_BYTES_MIN, modiff_incr, buf_compute_unchanged, &
                         buf_bytes, barf_if_buffer_read_only, deactivate_mark_variable, buf_next_char_len, &
                         current_buffer, set_buffer, set_buffer_if_live, buffer_live_p, set_buffer_internal
    use marker_fm, only: char_to_byte, byte_to_char, markers, clear_charpos_cache, copy_marker_at, &
                         marker_position, free_marker, buf_charpos_to_bytepos
    use undo_fm, only: record_insert, record_delete, record_first_change, undo_auto_undoable_change
    use intervals_fm, only: offset_intervals, graft_intervals_into_buffer, copy_intervals
    use textprop_fm, only: verify_interval_modification, report_interval_modification, inhibit_modification_hooks, &
                           interval_insert_behind_hooks, interval_insert_in_front_hooks
    use lisp_fm, only: value_type, xstring, string_intervals
    use eval_fm, only: hook_type, change_hook_type, after_change_hook_type
    use search_fm, only: update_search_regs
    use itree_fm, only: adjust_overlays_for_insert, adjust_overlays_for_delete, buffer_has_overlays, report_overlay_modification
    implicit none(type, external)
    private

    public :: move_gap_both, make_gap
    public :: insert, insert_char, insert_before_markers, insert_1_both
    public :: insert_and_inherit, insert_before_markers_and_inherit, insert_from_string, &
              insert_from_string_before_markers, insert_from_buffer, copy_text, make_gap_1, safe_del_range
    public :: del_range, del_range_1, del_range_byte, del_range_both, &
              del_range_2
    public :: modify_text, prepare_to_modify_buffer, prepare_to_modify_buffer_1
    public :: signal_before_change, signal_after_change
    public :: replace_range, replace_range_2
    public :: inhibit_modification_hooks, combine_after_change_execute

    !> Functions to call before each text change.
    !> Two arguments are passed to each function: the positions of
    !> the beginning and end of the range of old text to be changed.
    !> (For an insertion, the beginning and end are at the same place.)
    !> No information is given about the length of the text after the change.
    !>
    !> Buffer changes made while executing the `before_change_functions'
    !> don't call any before-change or after-change functions.
    !> That's because `inhibit_modification_hooks' is temporarily set true.
    !>
    !> If an unhandled error happens in running these functions,
    !> the variable's value remains empty.  That prevents the error
    !> from happening repeatedly and making Emacs nonfunctional.
    !> Port of buffer.c before-change-functions.
    type(change_hook_type), public, target :: before_change_functions = change_hook_type(name="before_change_functions")

    !> Functions to call after each text change.
    !> Three arguments are passed to each function: the positions of
    !> the beginning and end of the range of changed text,
    !> and the length in chars of the pre-change text replaced by that range.
    !> (For an insertion, the pre-change length is zero;
    !> for a deletion, that length is the number of chars deleted,
    !> and the post-change beginning and end are at the same place.)
    !>
    !> Buffer changes made while executing the `after_change_functions'
    !> don't call any before-change or after-change functions.
    !> That's because `inhibit_modification_hooks' is temporarily set true.
    !>
    !> If an unhandled error happens in running these functions,
    !> the variable's value remains empty.  That prevents the error
    !> from happening repeatedly and making Emacs nonfunctional.
    !> Port of buffer.c after-change-functions.
    type(after_change_hook_type), public, target :: after_change_functions = after_change_hook_type(name="after_change_functions")

    !> Functions to call before changing a buffer which is unmodified.
    !> The functions are run using the `run_hooks' function.
    !> Port of buffer.c first-change-hook.
    type(hook_type), public, target :: first_change_hook = hook_type(name="first_change_hook")

    !> Used internally by the `combine_after_change_calls' macro.
    !> Port of insdel.c combine-after-change-calls.
    logical, public :: combine_after_change_calls = .false.

    !> The changes recorded while `combine_after_change_calls' is true, one
    !> per column: the characters unchanged at the beginning of the buffer
    !> and at its end, and the change in length (insdel.c
    !> combine_after_change_list), and the buffer they were made in
    !> (combine_after_change_buffer).
    integer(int64), allocatable :: combine_list(:, :)
    integer :: combine_count = 0
    type(handle_type) :: combine_after_change_buffer

contains

    ! ------------------------------------------------------------------
    ! The gap
    ! ------------------------------------------------------------------

    !> Move the gap to `bytepos' (also char position `charpos').
    !> Port of insdel.c:move_gap_both.
    subroutine move_gap_both(charpos, bytepos)
        integer(int64), intent(in) :: charpos, bytepos

        if (bytepos < buffers(cur)%text%gpt_byte) then
            call gap_left(charpos, bytepos, .false.)
        else if (bytepos > buffers(cur)%text%gpt_byte) then
            call gap_right(charpos, bytepos)
        end if
    end subroutine move_gap_both

    !> Move the gap down to `bytepos'.  With `newgap', leave the unchanged
    !> bookkeeping alone.  Port of insdel.c:gap_left.
    subroutine gap_left(charpos, bytepos, newgap)
        integer(int64), intent(in) :: charpos, bytepos
        logical, intent(in) :: newgap
        integer(int64) :: n

        if (.not. newgap) call buf_compute_unchanged(buffers(cur), charpos, buffers(cur)%text%gpt)

        associate (t => buffers(cur)%text)
            ! Bytes [bytepos, gpt_byte) move up to the end of the gap.
            n = t%gpt_byte - bytepos

            if (n > 0) then
                t%beg(bytepos + t%gap_size:t%gpt_byte - 1 + t%gap_size) = t%beg(bytepos:t%gpt_byte - 1)
            end if

            t%gpt_byte = bytepos
            t%gpt = charpos
        end associate
    end subroutine gap_left

    !> Move the gap up to `bytepos'.  Port of insdel.c:gap_right.
    subroutine gap_right(charpos, bytepos)
        integer(int64), intent(in) :: charpos, bytepos
        integer(int64) :: n

        call buf_compute_unchanged(buffers(cur), charpos, buffers(cur)%text%gpt)

        associate (t => buffers(cur)%text)
            ! Bytes [gpt_byte, bytepos) move down from after the gap.
            n = bytepos - t%gpt_byte

            if (n > 0) then
                t%beg(t%gpt_byte:bytepos - 1) = t%beg(t%gpt_byte + t%gap_size:bytepos - 1 + t%gap_size)
            end if

            t%gpt = charpos
            t%gpt_byte = bytepos
        end associate
    end subroutine gap_right

    !> Grow the gap by `nbytes_added' (plus slack), or shrink it by
    !> `-nbytes_added' (never below GAP_BYTES_MIN).
    !> Port of insdel.c:make_gap, make_gap_larger and make_gap_smaller.
    subroutine make_gap(nbytes_added, status)
        integer(int64), intent(in) :: nbytes_added
        type(status_type), intent(out) :: status

        if (nbytes_added >= 0) then
            ! Grow by at least 1/64 of the buffer to avoid O(N^2) growth.
            call make_gap_larger(max(nbytes_added, (buffers(cur)%text%z - BEG)/64), status)
        else
            call make_gap_smaller(-nbytes_added, status)
        end if
    end subroutine make_gap

    !> Port of insdel.c:make_gap_larger (with insdel.c:buffer_overflow, the
    !> error, and buffer.c:enlarge_buffer_text, the reallocation).  Upstream enlarges the block and
    !> slides the gap; here the text is copied into a larger string with the
    !> gap already in place, which gives the same layout.
    subroutine make_gap_larger(nbytes_requested, status)
        integer(int64), intent(in) :: nbytes_requested
        type(status_type), intent(out) :: status
        integer(int64) :: current_size, nbytes_added, after, new_gap
        character(len=:), allocatable :: grown
        integer :: astat

        associate (t => buffers(cur)%text)
            current_size = t%z_byte - BEG + t%gap_size

            if (BUF_BYTES_MAX - current_size < nbytes_requested) then
                call set_error(status, FM_GENERIC_ERROR, "Maximum buffer size exceeded")
                return
            end if

            ! Get enough to last a while.
            nbytes_added = min(nbytes_requested + GAP_BYTES_DFL, BUF_BYTES_MAX - current_size)
            allocate (character(len=current_size + nbytes_added) :: grown, stat=astat)

            if (astat /= 0) then
                call set_error(status, FM_MEMORY_FULL)
                return
            end if

            new_gap = t%gap_size + nbytes_added
            after = t%z_byte - t%gpt_byte
            if (t%gpt_byte > 1) grown(1:t%gpt_byte - 1) = t%beg(1:t%gpt_byte - 1)

            if (after > 0) then
                grown(t%gpt_byte + new_gap:t%gpt_byte + new_gap + after - 1) = &
                    t%beg(t%gpt_byte + t%gap_size:t%gpt_byte + t%gap_size + after - 1)
            end if

            call move_alloc(grown, t%beg)
            t%gap_size = new_gap
        end associate
    end subroutine make_gap_larger

    !> Port of insdel.c:make_gap_smaller.
    subroutine make_gap_smaller(nbytes_requested, status)
        integer(int64), intent(in) :: nbytes_requested
        type(status_type), intent(out) :: status
        integer(int64) :: nbytes_removed, after, new_gap
        character(len=:), allocatable :: shrunk
        integer :: astat

        associate (t => buffers(cur)%text)
            nbytes_removed = nbytes_requested
            if (t%gap_size - nbytes_removed < GAP_BYTES_MIN) nbytes_removed = t%gap_size - GAP_BYTES_MIN
            if (nbytes_removed <= 0) return
            new_gap = t%gap_size - nbytes_removed
            allocate (character(len=len(t%beg, kind=int64) - nbytes_removed) :: shrunk, stat=astat)

            if (astat /= 0) then
                call set_error(status, FM_MEMORY_FULL)
                return
            end if

            after = t%z_byte - t%gpt_byte
            if (t%gpt_byte > 1) shrunk(1:t%gpt_byte - 1) = t%beg(1:t%gpt_byte - 1)

            if (after > 0) then
                shrunk(t%gpt_byte + new_gap:t%gpt_byte + new_gap + after - 1) = &
                    t%beg(t%gpt_byte + t%gap_size:t%gpt_byte + t%gap_size + after - 1)
            end if

            call move_alloc(shrunk, t%beg)
            t%gap_size = new_gap
        end associate
    end subroutine make_gap_smaller

    ! ------------------------------------------------------------------
    ! Insertion
    ! ------------------------------------------------------------------

    !> Insert `string' (in the buffer's representation) before point.
    !> Port of insdel.c:insert (and insdel.c:insert_string, the same for a C
    !> string).
    recursive subroutine insert(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call insert_text(string, .false., status)
    end subroutine insert

    !> Like `insert', but markers at point end up after the text.
    !> Port of insdel.c:insert_before_markers.
    recursive subroutine insert_before_markers(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call insert_text(string, .true., status)
    end subroutine insert_before_markers

    ! Not a port.
    recursive subroutine insert_text(string, before_markers, status)
        character(len=*), intent(in) :: string
        logical, intent(in) :: before_markers
        type(status_type), intent(out) :: status
        integer(int64) :: nchars, opoint

        if (len(string) == 0) return
        call chars_in_text(string, nchars, status)
        if (.not. status%ok()) return
        call insert_1_both(string, nchars, len(string, kind=int64), .false., .true., &
                           before_markers, status)
        if (.not. status%ok()) return
        opoint = buffers(cur)%pt - nchars
        call signal_after_change(opoint, 0_int64, nchars, status)
    end subroutine insert_text

    !> Insert character `c' before point.  Port of insdel.c:insert_char.
    recursive subroutine insert_char(c, status)
        integer(int32), intent(in) :: c
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: str
        integer(int32) :: n

        if (buffers(cur)%multibyte) then
            call char_string(c, str, n, status)
            if (.not. status%ok()) return
        else
            n = 1
            str(1:1) = char(iand(c, 255_int32))
        end if

        call insert(str(1:n), status)
    end subroutine insert_char

    !> Characters in `string' as the current buffer would count them.
    !> Port of character.c:chars_in_text.
    subroutine chars_in_text(string, nchars, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: nchars
        type(status_type), intent(out) :: status

        if (.not. buffers(cur)%multibyte) then
            nchars = len(string, kind=int64)
        else
            call multibyte_chars_in_text(string, nchars, status)
        end if
    end subroutine chars_in_text

    !> Insert `nchars' characters occupying the `nbytes' bytes of `string'
    !> before point.  `prepare' checks that the text may be modified and
    !> runs the before-change hooks; the caller runs the after-change
    !> hooks.  `inherit' makes the text inherit the properties of the text
    !> around it (by stickiness); `before_markers' moves the markers at
    !> point after the text.  Port of insdel.c:insert_1_both.
    recursive subroutine insert_1_both(string, nchars_in, nbytes, inherit, prepare, before_markers, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: nchars_in, nbytes
        logical, intent(in) :: inherit, prepare, before_markers
        type(status_type), intent(out) :: status
        integer(int64) :: nchars, pt

        if (nchars_in == 0) return
        nchars = nchars_in
        if (.not. buffers(cur)%multibyte) nchars = nbytes

        if (prepare) then
            ! Do this before moving and increasing the gap, because the
            ! before-change hooks might move the gap or make it smaller.
            pt = buffers(cur)%pt
            call prepare_to_modify_buffer(pt, pt, status)
            if (.not. status%ok()) return
        end if

        if (buffers(cur)%pt /= buffers(cur)%text%gpt) then
            call move_gap_both(buffers(cur)%pt, buffers(cur)%pt_byte)
        end if

        if (buffers(cur)%text%gap_size < nbytes) then
            call make_gap(nbytes - buffers(cur)%text%gap_size, status)
            if (.not. status%ok()) return
        end if

        call record_insert(buffers(cur)%pt, nchars)

        associate (b => buffers(cur), t => buffers(cur)%text)
            call modiff_incr(t%modiff, nchars)
            t%chars_modiff = t%modiff
            t%beg(t%gpt_byte:t%gpt_byte + nbytes - 1) = string(1:nbytes)
            t%gap_size = t%gap_size - nbytes
            t%gpt = t%gpt + nchars
            b%zv = b%zv + nchars
            t%z = t%z + nchars
            t%gpt_byte = t%gpt_byte + nbytes
            b%zv_byte = b%zv_byte + nbytes
            t%z_byte = t%z_byte + nbytes
            ! The insert may have been in the unchanged region.
            if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
        end associate

        call adjust_markers_for_insert(buffers(cur)%pt, buffers(cur)%pt_byte, &
                                       buffers(cur)%pt + nchars, buffers(cur)%pt_byte + nbytes, &
                                       before_markers)
        ! The inserted text gets an interval of its own: with no properties
        ! (upstream: offset_intervals, then set_text_properties to nil when
        ! not inheriting), or with those it inherits by stickiness.
        call offset_intervals(buffers(cur)%text%intervals, buffers(cur)%pt, nchars, inherit)
        call adjust_point(nchars, nbytes)
    end subroutine insert_1_both

    ! ------------------------------------------------------------------
    ! Inserting with text properties
    ! ------------------------------------------------------------------

    !> Make a gap of NBYTES bytes in buffer slot B.  Port of
    !> insdel.c:make_gap_1.
    subroutine make_gap_1(b, nbytes, status)
        integer(int64), intent(in) :: b, nbytes
        type(status_type), intent(out) :: status
        integer(int64) :: oldb

        oldb = cur
        call set_buffer_internal(b)
        call make_gap(nbytes, status)
        call set_buffer_internal(oldb)
    end subroutine make_gap_1

    !> The text FROM (multibyte if FROM_MULTIBYTE) as TO_MULTIBYTE says to
    !> store it: multibyte characters stored as unibyte keep their low 8
    !> bits; unibyte bytes stored as multibyte become their raw-byte
    !> characters (ASCII stays itself).  Port of insdel.c:copy_text.
    function copy_text(from, from_multibyte, to_multibyte) result(to)
        character(len=*), intent(in) :: from
        logical, intent(in) :: from_multibyte, to_multibyte
        character(len=:), allocatable :: to
        integer(int64) :: p, n
        integer(int32) :: c, thislen

        if (from_multibyte .eqv. to_multibyte) then
            to = from
        else if (from_multibyte) then
            allocate (character(len=len(from)) :: to)
            p = 1
            n = 0

            do while (p <= len(from, kind=int64))
                call string_char_and_length(from, p, c, thislen)
                if (c > 127) c = iand(c, 255_int32)
                n = n + 1
                to(n:n) = char(c)
                p = p + thislen
            end do

            to = to(1:n)
        else
            ! Convert single-byte to multibyte.
            to = str_to_multibyte(from)
        end if
    end function copy_text

    !> Insert `string' before point, inheriting the text properties of the
    !> text around it as its stickiness says.  Port of
    !> insdel.c:insert_and_inherit.
    recursive subroutine insert_and_inherit(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call insert_inheriting(string, .false., status)
    end subroutine insert_and_inherit

    !> Like `insert_and_inherit', but markers at point end up after the
    !> text.  Port of insdel.c:insert_before_markers_and_inherit.
    recursive subroutine insert_before_markers_and_inherit(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call insert_inheriting(string, .true., status)
    end subroutine insert_before_markers_and_inherit

    ! Not a port.
    recursive subroutine insert_inheriting(string, before_markers, status)
        character(len=*), intent(in) :: string
        logical, intent(in) :: before_markers
        type(status_type), intent(out) :: status
        integer(int64) :: nchars, opoint

        status = status_type()
        if (len(string) == 0) return
        call chars_in_text(string, nchars, status)
        if (.not. status%ok()) return
        call insert_1_both(string, nchars, len(string, kind=int64), .true., .true., before_markers, status)
        if (.not. status%ok()) return
        opoint = buffers(cur)%pt - nchars
        call signal_after_change(opoint, 0_int64, nchars, status)
    end subroutine insert_inheriting

    !> Insert the part of the text of STRING (a string value) consisting
    !> of the LENGTH characters starting at character POS (0-based) before
    !> point.  If the text of STRING has properties, copy them into the
    !> buffer (with INHERIT, add them to those the text inherits).  Port of
    !> insdel.c:insert_from_string.
    recursive subroutine insert_from_string(string, pos, length, inherit, status)
        type(value_type), intent(in) :: string
        integer(int64), intent(in) :: pos, length
        logical, intent(in) :: inherit
        type(status_type), intent(out) :: status
        integer(int64) :: opoint

        status = status_type()
        if (len(xstring(string)) == 0) return
        opoint = buffers(cur)%pt
        call insert_from_string_1(string, pos, length, inherit, .false., status)
        if (.not. status%ok()) return
        call signal_after_change(opoint, 0_int64, buffers(cur)%pt - opoint, status)
    end subroutine insert_from_string

    !> Like `insert_from_string' except that all markers pointing at the
    !> place where the insertion happens are adjusted to point after it.
    !> Port of insdel.c:insert_from_string_before_markers.
    recursive subroutine insert_from_string_before_markers(string, pos, length, inherit, status)
        type(value_type), intent(in) :: string
        integer(int64), intent(in) :: pos, length
        logical, intent(in) :: inherit
        type(status_type), intent(out) :: status
        integer(int64) :: opoint

        status = status_type()
        if (len(xstring(string)) == 0) return
        opoint = buffers(cur)%pt
        call insert_from_string_1(string, pos, length, inherit, .true., status)
        if (.not. status%ok()) return
        call signal_after_change(opoint, 0_int64, buffers(cur)%pt - opoint, status)
    end subroutine insert_from_string_before_markers

    !> Subroutine of the insertion functions above: insert the characters
    !> POS to POS + NCHARS of STRING (strings hold multibyte text) and graft
    !> their properties.  Port of insdel.c:insert_from_string_1.
    recursive subroutine insert_from_string_1(string, pos, nchars, inherit, before_markers, status)
        type(value_type), intent(in) :: string
        integer(int64), intent(in) :: pos, nchars
        logical, intent(in) :: inherit, before_markers
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, text
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        integer(int64) :: from_byte, to_byte, k, at

        status = status_type()
        if (nchars <= 0) return
        s = xstring(string)
        from_byte = string_char_to_byte(s, pos)
        to_byte = string_char_to_byte(s, pos + nchars)
        ! Copy the string text, perhaps converting between single-byte and
        ! multibyte.
        text = copy_text(s(from_byte:to_byte - 1), .true., buffers(cur)%multibyte)
        at = buffers(cur)%pt
        call insert_1_both(text, nchars, len(text, kind=int64), inherit, .true., before_markers, status)
        if (.not. status%ok()) return
        ! Get the intervals for the part of the string we are inserting.
        call string_intervals(string, starts, ends, plists)

        do k = 1, size(starts)
            starts(k) = starts(k) - pos
            ends(k) = ends(k) - pos
        end do

        ! Insert those intervals.
        call graft_intervals_into_buffer(buffers(cur)%text%intervals, buffers(cur)%text%z, starts, ends, plists, &
                                         at, nchars, inherit)
    end subroutine insert_from_string_1

    !> Byte index (1-based) in multibyte S of character CHARPOS (0-based).
    !> Not a port.
    integer(int64) function string_char_to_byte(s, charpos) result(b)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: charpos
        integer(int64) :: n
        integer(int32) :: c, l

        b = 1
        n = 0

        do while (n < charpos .and. b <= len(s, kind=int64))
            call string_char_and_length(s, b, c, l)
            b = b + l
            n = n + 1
        end do
    end function string_char_to_byte

    !> Insert before point NCHARS characters of buffer slot BUF, starting
    !> at character position CHARPOS, with their text properties (with
    !> INHERIT, added to those the text inherits).  Port of
    !> insdel.c:insert_from_buffer.
    recursive subroutine insert_from_buffer(buf, charpos, nchars, inherit, status)
        integer(int64), intent(in) :: buf, charpos, nchars
        logical, intent(in) :: inherit
        type(status_type), intent(out) :: status
        integer(int64) :: opoint

        opoint = buffers(cur)%pt
        call insert_from_buffer_1(buf, charpos, nchars, inherit, status)
        if (.not. status%ok()) return
        call signal_after_change(opoint, 0_int64, buffers(cur)%pt - opoint, status)
    end subroutine insert_from_buffer

    !> Subroutine of `insert_from_buffer'.  Port of
    !> insdel.c:insert_from_buffer_1.
    recursive subroutine insert_from_buffer_1(buf, from, nchars, inherit, status)
        integer(int64), intent(in) :: buf, from, nchars
        logical, intent(in) :: inherit
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        integer(int64) :: from_byte, to_byte, at

        status = status_type()
        if (nchars == 0) return
        ! Do this before moving and increasing the gap, because the
        ! before-change hooks might move the gap or make it smaller.
        at = buffers(cur)%pt
        call prepare_to_modify_buffer(at, at, status)
        if (.not. status%ok()) return
        from_byte = buf_charpos_to_bytepos(buf, from)
        to_byte = buf_charpos_to_bytepos(buf, from + nchars)
        text = copy_text(buf_bytes(buffers(buf), from_byte, to_byte), buffers(buf)%multibyte, buffers(cur)%multibyte)
        ! Get the intervals for the part of the text we are inserting.
        call copy_intervals(buffers(buf)%text%intervals, from, nchars, starts, ends, plists)
        at = buffers(cur)%pt
        call insert_1_both(text, nchars, len(text, kind=int64), inherit, .false., .false., status)
        if (.not. status%ok()) return
        ! Insert those intervals.
        call graft_intervals_into_buffer(buffers(cur)%text%intervals, buffers(cur)%text%z, starts, ends, plists, &
                                         at, nchars, inherit)
    end subroutine insert_from_buffer_1

    !> Delete FROM..TO like `del_range', but return true instead of an
    !> error if the deletion fails.  Port of insdel.c:safe_del_range (with
    !> insdel.c:safe_del_range_1, the deletion, and insdel.c:safe_del_range_2,
    !> the true on failure).
    recursive logical function safe_del_range(from, to) result(failed)
        integer(int64), intent(in) :: from, to
        type(status_type) :: status

        call del_range(from, to, status)
        failed = .not. status%ok()
    end function safe_del_range

    !> Relocate the current buffer's markers for an insertion spanning
    !> `from'..`to'.  Markers at the insertion point advance only if their
    !> insertion type is t or `before_markers' is set.
    !> Port of insdel.c:adjust_markers_for_insert.
    subroutine adjust_markers_for_insert(from, from_byte, to, to_byte, before_markers)
        integer(int64), intent(in) :: from, from_byte, to, to_byte
        logical, intent(in) :: before_markers
        integer(int64) :: m, nchars, nbytes

        nchars = to - from
        nbytes = to_byte - from_byte
        m = buffers(cur)%text%markers

        do while (m /= 0)
            associate (mk => markers(m))
                if (mk%bytepos == from_byte) then
                    if (mk%insertion_type .or. before_markers) then
                        mk%bytepos = to_byte
                        mk%charpos = to
                    end if
                else if (mk%bytepos > from_byte) then
                    mk%bytepos = mk%bytepos + nbytes
                    mk%charpos = mk%charpos + nchars
                end if

                m = mk%next
            end associate
        end do

        call adjust_overlays_for_insert(from, to - from, before_markers)
    end subroutine adjust_markers_for_insert

    !> Relocate the current buffer's markers for a deletion of
    !> `from'..`to': markers inside move to `from', markers after shift.
    !> Port of insdel.c:adjust_markers_for_delete.
    subroutine adjust_markers_for_delete(from, from_byte, to, to_byte)
        integer(int64), intent(in) :: from, from_byte, to, to_byte
        integer(int64) :: m

        m = buffers(cur)%text%markers

        do while (m /= 0)
            associate (mk => markers(m))
                if (mk%charpos > to) then
                    mk%charpos = mk%charpos - (to - from)
                    mk%bytepos = mk%bytepos - (to_byte - from_byte)
                else if (mk%charpos > from) then
                    mk%charpos = from
                    mk%bytepos = from_byte
                end if

                m = mk%next
            end associate
        end do

        call adjust_overlays_for_delete(from, to - from)
    end subroutine adjust_markers_for_delete

    !> Move point by `nchars'/`nbytes'.  Port of insdel.c:adjust_point.
    subroutine adjust_point(nchars, nbytes)
        integer(int64), intent(in) :: nchars, nbytes

        buffers(cur)%pt = buffers(cur)%pt + nchars
        buffers(cur)%pt_byte = buffers(cur)%pt_byte + nbytes
    end subroutine adjust_point

    ! ------------------------------------------------------------------
    ! Deletion
    ! ------------------------------------------------------------------

    !> Delete characters `from' up to `to', clamped to the accessible
    !> region; nothing if `to <= from'.  Port of insdel.c:del_range.
    recursive subroutine del_range(from, to, status)
        integer(int64), intent(in) :: from, to
        type(status_type), intent(out) :: status

        call del_range_1(from, to, .true., status)
    end subroutine del_range

    !> Like `del_range'; `prepare' checks that the text may be modified
    !> and runs the before-change hooks.  With `deleted' present, it
    !> receives the deleted text.  Port of insdel.c:del_range_1.
    recursive subroutine del_range_1(from_in, to_in, prepare, status, deleted)
        integer(int64), intent(in) :: from_in, to_in
        logical, intent(in) :: prepare
        type(status_type), intent(out) :: status
        character(len=:), allocatable, intent(out), optional :: deleted
        integer(int64) :: from, to, from_byte, to_byte, range_length

        from = max(from_in, buffers(cur)%begv)
        to = min(to_in, buffers(cur)%zv)

        if (to <= from) then
            if (present(deleted)) deleted = ""
            return
        end if

        if (prepare) then
            range_length = to - from
            call prepare_preserving(from, to, status)
            if (.not. status%ok()) return
            to = min(buffers(cur)%zv, from + range_length)
        end if

        from_byte = char_to_byte(from)
        to_byte = char_to_byte(to)
        call del_range_2(from, from_byte, to, to_byte, deleted)
        call signal_after_change(from, to - from, 0_int64, status)
    end subroutine del_range_1

    !> Like `del_range_1', with byte positions.  Port of insdel.c:del_range_byte.
    recursive subroutine del_range_byte(from_byte_in, to_byte_in, status)
        integer(int64), intent(in) :: from_byte_in, to_byte_in
        type(status_type), intent(out) :: status
        integer(int64) :: from, to, from_byte, to_byte

        from_byte = max(from_byte_in, buffers(cur)%begv_byte)
        to_byte = min(to_byte_in, buffers(cur)%zv_byte)
        if (to_byte <= from_byte) return
        from = byte_to_char(from_byte)
        to = byte_to_char(to_byte)
        call prepare_for_deletion(from, from_byte, to, to_byte, status)
        if (.not. status%ok()) return
        call del_range_2(from, from_byte, to, to_byte)
        call signal_after_change(from, to - from, 0_int64, status)
    end subroutine del_range_byte

    !> Like `del_range_1', with both kinds of position given.
    !> Port of insdel.c:del_range_both.
    recursive subroutine del_range_both(from_in, from_byte_in, to_in, to_byte_in, prepare, status)
        integer(int64), intent(in) :: from_in, from_byte_in, to_in, to_byte_in
        logical, intent(in) :: prepare
        type(status_type), intent(out) :: status
        integer(int64) :: from, to, from_byte, to_byte

        status = status_type()
        from_byte = max(from_byte_in, buffers(cur)%begv_byte)
        to_byte = min(to_byte_in, buffers(cur)%zv_byte)
        if (to_byte <= from_byte) return
        from = max(from_in, buffers(cur)%begv)
        to = min(to_in, buffers(cur)%zv)

        if (prepare) then
            call prepare_for_deletion(from, from_byte, to, to_byte, status)
            if (.not. status%ok()) return
        end if

        call del_range_2(from, from_byte, to, to_byte)
        call signal_after_change(from, to - from, 0_int64, status)
    end subroutine del_range_both

    !> Run `prepare_to_modify_buffer' for deleting FROM..TO and correct
    !> the positions for what the before-change hooks did: FROM is kept
    !> in a marker, TO stays as far after it, within the accessible
    !> region.  Port of insdel.c:del_range_byte and
    !> insdel.c:del_range_both (the part before the deletion).
    recursive subroutine prepare_for_deletion(from, from_byte, to, to_byte, status)
        integer(int64), intent(inout) :: from, from_byte, to, to_byte
        type(status_type), intent(out) :: status
        integer(int64) :: old_from, old_to, range_length

        old_from = from
        old_to = buffers(cur)%text%z - to
        range_length = to - from
        call prepare_preserving(from, to, status)
        if (.not. status%ok()) return
        to = from + range_length
        if (old_from /= from) from_byte = char_to_byte(from)

        if (to > buffers(cur)%zv) then
            to = buffers(cur)%zv
            to_byte = buffers(cur)%zv_byte
        else if (old_to == buffers(cur)%text%z - to) then
            to_byte = char_to_byte(to)
        end if
    end subroutine prepare_for_deletion

    !> Delete the text between two valid positions, given as both char and
    !> byte positions.  With `deleted' present, it receives the deleted text.
    !> Port of insdel.c:del_range_2.
    subroutine del_range_2(from, from_byte, to, to_byte, deleted)
        integer(int64), intent(in) :: from, from_byte, to, to_byte
        character(len=:), allocatable, intent(out), optional :: deleted
        integer(int64) :: nchars_del, nbytes_del

        nchars_del = to - from
        nbytes_del = to_byte - from_byte
        ! Make sure the gap is somewhere in or next to what we are deleting.
        if (from > buffers(cur)%text%gpt) call gap_right(from, from_byte)
        if (to < buffers(cur)%text%gpt) call gap_left(to, to_byte, .false.)
        if (present(deleted)) deleted = buf_bytes(buffers(cur), from_byte, to_byte)
        if (buffers(cur)%undo_enabled) call record_delete(from, buf_bytes(buffers(cur), from_byte, to_byte), nchars_del)
        ! Relocate all markers pointing into the new, larger gap.
        call adjust_markers_for_delete(from, from_byte, to, to_byte)

        associate (b => buffers(cur), t => buffers(cur)%text)
            call modiff_incr(t%modiff, nchars_del)
            t%chars_modiff = t%modiff

            ! Relocate point as if it were a marker.
            if (from < b%pt) then
                b%pt = b%pt + (from - min(b%pt, to))
                b%pt_byte = b%pt_byte + (from_byte - min(b%pt_byte, to_byte))
            end if

            call offset_intervals(t%intervals, from, -nchars_del)
            t%gap_size = t%gap_size + nbytes_del
            b%zv = b%zv - nchars_del
            t%z = t%z - nchars_del
            b%zv_byte = b%zv_byte - nbytes_del
            t%z_byte = t%z_byte - nbytes_del
            t%gpt = from
            t%gpt_byte = from_byte
            if (t%gpt - BEG < t%beg_unchanged) t%beg_unchanged = t%gpt - BEG
            if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
        end associate
    end subroutine del_range_2

    ! ------------------------------------------------------------------
    ! Replacement
    ! ------------------------------------------------------------------

    !> Replace the text from character positions FROM to TO with NEW
    !> (multibyte text).  With `run_mod_hooks', check that the text may be
    !> modified and run the before- and after-change hooks.  With
    !> `adjust_match_data', adjust the match data for the replacement
    !> before the after-change hooks run.  Markers in the replaced text
    !> move to FROM; point moves as if it were a marker, except that point
    !> at TO stays after the new text.  Port of insdel.c:replace_range for
    !> a string (INHERIT false).
    recursive subroutine replace_range(from_in, to_in, new, run_mod_hooks, status, adjust_match_data)
        integer(int64), intent(in) :: from_in, to_in
        character(len=*), intent(in) :: new
        logical, intent(in) :: run_mod_hooks
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: adjust_match_data
        integer(int64) :: from, to, from_byte, to_byte, nchars_del, nbytes_del, inschars, insbytes, range_length
        character(len=:), allocatable :: deletion
        logical :: have_deletion

        status = status_type()
        from = from_in
        to = to_in
        call chars_in_text(new, inschars, status)
        if (.not. status%ok()) return
        insbytes = len(new, kind=int64)

        if (run_mod_hooks) then
            range_length = to - from
            call prepare_preserving(from, to, status)
            if (.not. status%ok()) return
            to = from + range_length
        end if

        ! Make args be valid.
        from = max(from, buffers(cur)%begv)
        to = min(to, buffers(cur)%zv)
        from_byte = char_to_byte(from)
        to_byte = char_to_byte(to)
        nchars_del = to - from
        nbytes_del = to_byte - from_byte
        if (nbytes_del <= 0 .and. inschars == 0) return
        ! Make sure the gap is somewhere in or next to what we are deleting.
        if (from > buffers(cur)%text%gpt) call gap_right(from, from_byte)
        if (to < buffers(cur)%text%gpt) call gap_left(to, to_byte, .false.)
        have_deletion = buffers(cur)%undo_enabled
        if (have_deletion) deletion = buf_bytes(buffers(cur), from_byte, to_byte)

        associate (b => buffers(cur), t => buffers(cur)%text)
            t%gap_size = t%gap_size + nbytes_del
            b%zv = b%zv - nchars_del
            t%z = t%z - nchars_del
            b%zv_byte = b%zv_byte - nbytes_del
            t%z_byte = t%z_byte - nbytes_del
            t%gpt = from
            t%gpt_byte = from_byte
            if (t%gpt - BEG < t%beg_unchanged) t%beg_unchanged = t%gpt - BEG
            if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
        end associate

        if (buffers(cur)%text%gap_size < insbytes) then
            call make_gap(insbytes - buffers(cur)%text%gap_size, status)
            if (.not. status%ok()) return
        end if

        ! Copy the string text into the buffer.
        associate (t => buffers(cur)%text)
            if (insbytes > 0) t%beg(t%gpt_byte:t%gpt_byte + insbytes - 1) = new
        end associate

        ! Record the insertion first, so that when we undo, the deletion
        ! will be undone first.  Thus, undo will insert before deleting,
        ! and thus will keep the markers before and after this text
        ! separate.
        if (have_deletion) then
            call record_insert(from + nchars_del, inschars)
            call record_delete(from, deletion, nchars_del)
        end if

        associate (b => buffers(cur), t => buffers(cur)%text)
            t%gap_size = t%gap_size - insbytes
            t%gpt = t%gpt + inschars
            b%zv = b%zv + inschars
            t%z = t%z + inschars
            t%gpt_byte = t%gpt_byte + insbytes
            b%zv_byte = b%zv_byte + insbytes
            t%z_byte = t%z_byte + insbytes
        end associate

        ! Adjust markers for the deletion and the insertion.
        call adjust_markers_for_replace(from, from_byte, nchars_del, nbytes_del, inschars, insbytes)
        call offset_intervals(buffers(cur)%text%intervals, from, inschars - nchars_del)

        ! Relocate point as if it were a marker (mimic insert when
        ! FROM == TO == PT).
        associate (b => buffers(cur))
            if (from < b%pt .or. b%pt == to) then
                call adjust_point(from + inschars - min(b%pt, to), from_byte + insbytes - min(b%pt_byte, to_byte))
            end if
        end associate

        associate (t => buffers(cur)%text)
            call modiff_incr(t%modiff, nchars_del + inschars)
            t%chars_modiff = t%modiff
        end associate

        if (present(adjust_match_data)) then
            if (adjust_match_data) call update_search_regs(from, to, from + inschars)
        end if

        if (run_mod_hooks) call signal_after_change(from, nchars_del, buffers(cur)%text%gpt - from, status)
    end subroutine replace_range

    !> Replace the text from character positions FROM to TO with the
    !> INSCHARS characters of INS.  Keep the text properties that applied
    !> to the old characters.  With `markers', relocate markers; otherwise
    !> only their byte positions are corrected.  Unlike most functions at
    !> this level, this never checks whether the text may be modified and
    !> records no undo and runs no change hooks: the caller does that for
    !> the whole region.  Port of
    !> insdel.c:replace_range_2.
    subroutine replace_range_2(from, from_byte, to, to_byte, ins, inschars, markers, status)
        integer(int64), intent(in) :: from, from_byte, to, to_byte, inschars
        character(len=*), intent(in) :: ins
        logical, intent(in) :: markers
        type(status_type), intent(out) :: status
        integer(int64) :: nbytes_del, nchars_del, insbytes

        status = status_type()
        nchars_del = to - from
        nbytes_del = to_byte - from_byte
        insbytes = len(ins, kind=int64)
        if (nbytes_del <= 0 .and. insbytes == 0) return
        ! Make sure the gap is somewhere in or next to what we are deleting.
        if (from > buffers(cur)%text%gpt) call gap_right(from, from_byte)
        if (to < buffers(cur)%text%gpt) call gap_left(to, to_byte, .false.)

        associate (b => buffers(cur), t => buffers(cur)%text)
            t%gap_size = t%gap_size + nbytes_del
            b%zv = b%zv - nchars_del
            t%z = t%z - nchars_del
            b%zv_byte = b%zv_byte - nbytes_del
            t%z_byte = t%z_byte - nbytes_del
            t%gpt = from
            t%gpt_byte = from_byte
            if (t%gpt - BEG < t%beg_unchanged) t%beg_unchanged = t%gpt - BEG
            if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
        end associate

        if (buffers(cur)%text%gap_size < insbytes) then
            call make_gap(insbytes - buffers(cur)%text%gap_size, status)
            if (.not. status%ok()) return
        end if

        ! Copy the replacement text into the buffer.
        associate (b => buffers(cur), t => buffers(cur)%text)
            if (insbytes > 0) t%beg(t%gpt_byte:t%gpt_byte + insbytes - 1) = ins
            t%gap_size = t%gap_size - insbytes
            t%gpt = t%gpt + inschars
            b%zv = b%zv + inschars
            t%z = t%z + inschars
            t%gpt_byte = t%gpt_byte + insbytes
            b%zv_byte = b%zv_byte + insbytes
            t%z_byte = t%z_byte + insbytes
        end associate

        ! Adjust markers for the deletion and the insertion.
        if (.not. (nchars_del == 1 .and. inschars == 1 .and. nbytes_del == insbytes)) then
            if (markers) then
                call adjust_markers_for_replace(from, from_byte, nchars_del, nbytes_del, inschars, insbytes)
            else
                ! The character positions of the markers remain intact, but
                ! we still need to update their byte positions.
                call adjust_markers_bytepos(from, from_byte)
            end if
        end if

        call offset_intervals(buffers(cur)%text%intervals, from, inschars - nchars_del)

        ! Relocate point as if it were a marker.
        associate (b => buffers(cur))
            if (from < b%pt .and. (nchars_del /= inschars .or. nbytes_del /= insbytes)) then
                if (b%pt < to) then
                    ! PT was within the deleted text.  Move it to FROM.
                    call adjust_point(from - b%pt, from_byte - b%pt_byte)
                else
                    call adjust_point(inschars - nchars_del, insbytes - nbytes_del)
                end if
            end if
        end associate

        associate (t => buffers(cur)%text)
            call modiff_incr(t%modiff, nchars_del + inschars)
            t%chars_modiff = t%modiff
        end associate
    end subroutine replace_range_2

    !> Recompute the byte position of each marker after FROM from its
    !> character position.  Port of insdel.c:adjust_markers_bytepos (TO_Z
    !> true), with insdel.c:count_bytes (the inner loop, counting forward
    !> from FROM).
    subroutine adjust_markers_bytepos(from, from_byte)
        integer(int64), intent(in) :: from, from_byte
        integer(int64) :: m, pos, pos_byte

        m = buffers(cur)%text%markers

        do while (m /= 0)
            associate (mk => markers(m))
                if (mk%bytepos > from_byte) then
                    pos = from
                    pos_byte = from_byte

                    do while (pos < mk%charpos .and. pos_byte < buffers(cur)%text%z_byte)
                        pos_byte = pos_byte + buf_next_char_len(buffers(cur), pos_byte)
                        pos = pos + 1
                    end do

                    mk%bytepos = pos_byte
                end if

                m = mk%next
            end associate
        end do

        ! Make sure cached charpos/bytepos is invalid.
        call clear_charpos_cache(cur)
    end subroutine adjust_markers_bytepos

    !> Relocate markers for replacing OLD_CHARS characters at FROM with
    !> NEW_CHARS: markers in the old text move to FROM, markers after it
    !> shift.  Port of insdel.c:adjust_markers_for_replace.
    subroutine adjust_markers_for_replace(from, from_byte, old_chars, old_bytes, new_chars, new_bytes)
        integer(int64), intent(in) :: from, from_byte, old_chars, old_bytes, new_chars, new_bytes
        integer(int64) :: m, prev_to_byte, diff_chars, diff_bytes

        if (old_chars == 0) then
            ! Just an insertion: markers at FROM may need to move or not
            ! depending on their marker type.
            call adjust_markers_for_insert(from, from_byte, from + new_chars, from_byte + new_bytes, .false.)
            return
        end if

        prev_to_byte = from_byte + old_bytes
        diff_chars = new_chars - old_chars
        diff_bytes = new_bytes - old_bytes
        m = buffers(cur)%text%markers

        do while (m /= 0)
            associate (mk => markers(m))
                if (mk%bytepos >= prev_to_byte) then
                    mk%charpos = mk%charpos + diff_chars
                    mk%bytepos = mk%bytepos + diff_bytes
                else if (mk%bytepos > from_byte) then
                    mk%charpos = from
                    mk%bytepos = from_byte
                end if

                m = mk%next
            end associate
        end do

        call adjust_overlays_for_insert(from + old_chars, new_chars, .true.)
        call adjust_overlays_for_delete(from, old_chars)
    end subroutine adjust_markers_for_replace

    ! ------------------------------------------------------------------
    ! Modification bookkeeping
    ! ------------------------------------------------------------------

    !> Announce a change to characters `start'..`end' that does not insert
    !> or delete (e.g. case changes): check that the text may be modified
    !> and run the before-change hooks.  The caller runs the after-change
    !> hooks.  Port of insdel.c:modify_text.
    recursive subroutine modify_text(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call prepare_to_modify_buffer(start, end, status)
        if (.not. status%ok()) return
        call buf_compute_unchanged(buffers(cur), start - 1, end)
        if (buffers(cur)%text%modiff <= buffers(cur)%text%save_modiff) call record_first_change()

        associate (t => buffers(cur)%text)
            call modiff_incr(t%modiff, end - start)
            t%chars_modiff = t%modiff
        end associate
    end subroutine modify_text

    !> Run `undo_auto__undoable_change' unless the current buffer keeps no
    !> undo information.  Port of insdel.c:run_undoable_change.
    subroutine run_undoable_change()
        if (.not. buffers(cur)%undo_enabled) return
        call undo_auto_undoable_change()
    end subroutine run_undoable_change

    !> Check that it is okay to modify the buffer between START and END,
    !> which are char positions: the buffer's read-only state and
    !> `read_only' text properties, running the text properties'
    !> modification hooks.  Then, unless `inhibit_modification_hooks', run
    !> the before-change hooks and set `deactivate_mark'.  If PRESERVE is
    !> present, relocate it for what the hooks do by holding its value
    !> temporarily in a marker.  Port of insdel.c:prepare_to_modify_buffer_1
    !> (locking the visited file and saving the region for
    !> `select_active_regions' are deferred).
    recursive subroutine prepare_to_modify_buffer_1(start, end, status, preserve)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(inout), optional :: preserve
        type(handle_type) :: preserve_marker
        type(status_type) :: st

        call barf_if_buffer_read_only(status, start)
        if (.not. status%ok()) return
        call run_undoable_change()
        buffers(cur)%text%redisplay = .true.

        if (buffers(cur)%text%intervals%n > 0) then
            if (present(preserve)) then
                call copy_marker_at(preserve, preserve_marker, .false., st)
                call verify_interval_modification(start, end, status)
                call marker_position(preserve_marker, preserve, st)
                call free_marker(preserve_marker, st)
            else
                call verify_interval_modification(start, end, status)
            end if

            if (.not. status%ok()) return
        end if

        if (inhibit_modification_hooks%value) return
        call signal_before_change(start, end, status, preserve)
        if (.not. status%ok()) return
        deactivate_mark_variable = .true.
    end subroutine prepare_to_modify_buffer_1

    !> Like `prepare_to_modify_buffer_1', when the buffer text will be
    !> modified.  Port of insdel.c:prepare_to_modify_buffer (no region
    !> caches to invalidate yet).
    recursive subroutine prepare_to_modify_buffer(start, end, status, preserve)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(inout), optional :: preserve

        call prepare_to_modify_buffer_1(start, end, status, preserve)
    end subroutine prepare_to_modify_buffer

    !> `prepare_to_modify_buffer' for FROM..TO, relocating FROM for what
    !> the before-change hooks do (upstream passes &from as PRESERVE_PTR).
    !> Not a port.
    recursive subroutine prepare_preserving(from, to, status)
        integer(int64), intent(inout) :: from
        integer(int64), intent(in) :: to
        type(status_type), intent(out) :: status
        integer(int64) :: start

        start = from
        call prepare_to_modify_buffer(start, to, status, from)
    end subroutine prepare_preserving

    !> Signal a change to the buffer immediately before it happens.
    !> START_INT and END_INT are the bounds of the text to be changed.
    !> If PRESERVE is present, relocate it by holding its value
    !> temporarily in a marker.  `first_change_hook' runs first if the
    !> buffer is unmodified, then `before_change_functions', both with
    !> `inhibit_modification_hooks' true; positions are kept in markers
    !> while they run.  An error in `before_change_functions' empties it
    !> (in the current buffer) and is returned.  Port of
    !> insdel.c:signal_before_change, with insdel.c:reset_var_on_error (the
    !> hook emptied on an error).  The overlays' modification hooks run
    !> last.
    recursive subroutine signal_before_change(start_int, end_int, status, preserve)
        integer(int64), intent(in) :: start_int, end_int
        type(status_type), intent(out) :: status
        integer(int64), intent(inout), optional :: preserve
        type(handle_type) :: start_marker, end_marker, preserve_marker
        type(status_type) :: st
        integer(int64) :: s, e
        logical :: have_start_end, have_preserve, saved_inhibit

        status = status_type()
        have_start_end = .false.
        have_preserve = .false.
        saved_inhibit = inhibit_modification_hooks%value
        inhibit_modification_hooks%value = .true.

        ! If buffer is unmodified, run a special hook for that case.  The
        ! check for an empty hook is just a minor optimization.
        if (buffers(cur)%text%save_modiff >= buffers(cur)%text%modiff) then
            if (.not. first_change_hook%empty()) then
                call preserve_value()
                call preserve_start_end()
                call first_change_hook%run(status)
            end if
        end if

        ! Now run the before-change-functions if any.
        if (status%ok()) then
            if (.not. before_change_functions%empty()) then
                call preserve_value()
                call preserve_start_end()
                s = start_int
                e = end_int

                if (have_start_end) then
                    call marker_position(start_marker, s, st)
                    call marker_position(end_marker, e, st)
                end if

                call before_change_functions%run(s, e, status)
                ! Reset before-change-functions to nil on an error.
                if (.not. status%ok()) call before_change_functions%clear()
            end if
        end if

        if (status%ok() .and. buffer_has_overlays()) then
            call preserve_value()
            s = start_int
            e = end_int

            if (have_start_end) then
                call marker_position(start_marker, s, st)
                call marker_position(end_marker, e, st)
            end if

            call report_overlay_modification(s, e, .false., s, e, status)
        end if

        if (have_start_end) then
            call free_marker(start_marker, st)
            call free_marker(end_marker, st)
        end if

        if (have_preserve) then
            if (status%ok()) call marker_position(preserve_marker, preserve, st)
            call free_marker(preserve_marker, st)
        end if

        inhibit_modification_hooks%value = saved_inhibit

    contains

        !> PRESERVE_VALUE.
        subroutine preserve_value()
            if (.not. present(preserve) .or. have_preserve) return
            call copy_marker_at(preserve, preserve_marker, .false., st)
            have_preserve = .true.
        end subroutine preserve_value

        !> PRESERVE_START_END.
        subroutine preserve_start_end()
            if (have_start_end) return
            call copy_marker_at(start_int, start_marker, .false., st)
            call copy_marker_at(end_int, end_marker, .false., st)
            have_start_end = .true.
        end subroutine preserve_start_end
    end subroutine signal_before_change

    !> Signal a change immediately after it happens.  CHARPOS is the
    !> character position of the start of the changed text.  LENDEL is
    !> the number of characters of the text before the change.  (Not the
    !> whole buffer; just the part that was changed.)  LENINS is the
    !> number of characters in that part of the text after the change.
    !> Runs `after_change_functions' with `inhibit_modification_hooks'
    !> true, unless it is true already (an error empties the hook in the
    !> current buffer and is returned), then the overlays' modification
    !> hooks and, after an insertion, the text properties' insert hooks.
    !> While `combine_after_change_calls' is true and nothing else needs
    !> the change reported at once, the change is only recorded, for
    !> `combine_after_change_execute'.  Port of insdel.c:signal_after_change
    !> (`syntax_ppss_flush_cache' is never on `before_change_functions'
    !> here, so only an empty hook lets the change be deferred).
    recursive subroutine signal_after_change(charpos, lendel, lenins, status)
        integer(int64), intent(in) :: charpos, lendel, lenins
        type(status_type), intent(out) :: status
        type(value_type) :: save_insert_behind_hooks, save_insert_in_front_hooks
        logical :: saved_inhibit

        status = status_type()
        if (inhibit_modification_hooks%value) return

        ! If we are deferring calls to the after-change functions and there
        ! are no before-change functions, just record the args that we were
        ! going to use.
        if (combine_after_change_calls .and. before_change_functions%empty() .and. .not. buffer_has_overlays()) then
            if (combine_count > 0) then
                if (.not. same_buffer(combine_after_change_buffer)) call combine_after_change_execute(status)
                if (.not. status%ok()) return
            end if

            call combine_record(charpos - BEG, buffers(cur)%text%z - (charpos - lendel + lenins), lenins - lendel)
            combine_after_change_buffer = current_buffer()
            return
        end if

        ! Save and restore the insert-*-hooks, because other hooks like
        ! after-change-functions, called below, could clobber them if they
        ! manipulate text properties.
        save_insert_behind_hooks = interval_insert_behind_hooks
        save_insert_in_front_hooks = interval_insert_in_front_hooks

        if (combine_count > 0) then
            call combine_after_change_execute(status)
            if (.not. status%ok()) return
        end if

        saved_inhibit = inhibit_modification_hooks%value
        inhibit_modification_hooks%value = .true.

        if (.not. after_change_functions%empty()) then
            call after_change_functions%run(charpos, charpos + lenins, lendel, status)

            ! Reset after-change-functions to nil on an error.
            if (.not. status%ok()) then
                call after_change_functions%clear()
                inhibit_modification_hooks%value = saved_inhibit
                return
            end if
        end if

        interval_insert_behind_hooks = save_insert_behind_hooks
        interval_insert_in_front_hooks = save_insert_in_front_hooks

        if (buffer_has_overlays()) then
            call report_overlay_modification(charpos, charpos + lenins, .true., charpos, charpos + lenins, status, lendel)
        end if

        ! After an insertion, call the text properties insert-behind-hooks
        ! or insert-in-front-hooks.
        if (status%ok() .and. lendel == 0) call report_interval_modification(charpos, charpos + lenins, status)
        inhibit_modification_hooks%value = saved_inhibit
    end subroutine signal_after_change

    !> Whether H is the current buffer.  Not a port.
    logical function same_buffer(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: c

        c = current_buffer()
        same_buffer = c%index == h%index .and. c%generation == h%generation
    end function same_buffer

    !> Push the change (BEG, END, CHANGE) on combine_after_change_list.
    !> Not a port.
    subroutine combine_record(beg, end, change)
        integer(int64), intent(in) :: beg, end, change
        integer(int64), allocatable :: grown(:, :)

        if (.not. allocated(combine_list)) allocate (combine_list(3, 16))

        if (combine_count == size(combine_list, 2)) then
            allocate (grown(3, 2*combine_count))
            grown(:, 1:combine_count) = combine_list(:, 1:combine_count)
            call move_alloc(grown, combine_list)
        end if

        combine_count = combine_count + 1
        combine_list(:, combine_count) = [beg, end, change]
    end subroutine combine_record

    !> Set `combine_after_change_calls' back to VAL.  Port of
    !> insdel.c:Fcombine_after_change_execute_1.
    subroutine combine_after_change_execute_1(val)
        logical, intent(in) :: val

        combine_after_change_calls = val
    end subroutine combine_after_change_execute_1

    !> This function is for use internally in the function
    !> `combine_after_change_calls'.  It reports the changes recorded while
    !> `combine_after_change_calls' was true as one change covering them
    !> all, in the buffer where they were made.  Port of
    !> insdel.c:Fcombine_after_change_execute (compositions are not ported,
    !> so there are none to update).
    recursive subroutine combine_after_change_execute(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: nbeg, nend, change, begpos, endpos, k
        logical :: saved

        status = status_type()
        if (combine_count == 0) return

        ! It is rare for combine_after_change_buffer to be invalid, but
        ! possible.
        if (.not. buffer_live_p(combine_after_change_buffer)) then
            combine_count = 0
            return
        end if

        old = current_buffer()
        call set_buffer(combine_after_change_buffer, st)
        ! # chars unchanged at beginning of buffer.
        nbeg = buffers(cur)%text%z - BEG
        ! # chars unchanged at end of buffer.
        nend = nbeg
        ! Total amount of insertion (negative for deletion).
        change = 0

        ! Scan the various individual changes, accumulating the range info
        ! in BEG, END and CHANGE.
        do k = 1, combine_count
            change = change + combine_list(3, k)
            if (combine_list(1, k) < nbeg) nbeg = combine_list(1, k)
            if (combine_list(2, k) < nend) nend = combine_list(2, k)
        end do

        ! Get the current start and end positions of the range that was
        ! changed.
        begpos = BEG + nbeg
        endpos = buffers(cur)%text%z - nend
        ! We are about to handle these, so discard them.
        combine_count = 0
        ! Now run the after-change functions for real.
        saved = combine_after_change_calls
        call signal_after_change(begpos, endpos - begpos - change, endpos - begpos, status)
        call combine_after_change_execute_1(saved)
        call set_buffer_if_live(old)
    end subroutine combine_after_change_execute
end module insdel_fm
