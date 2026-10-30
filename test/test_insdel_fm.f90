! test_insdel_fm.f90 -- tests for insdel_fm (with the change hooks) and
! marker_fm position conversion.
! Ports GNU Emacs 31.1 test/src/editfns-tests.el in part; only the
! change-hook cases; upstream has no insdel tests.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Upstream has no insdel-tests.el; these check the gap buffer directly,
!> and the change hooks (with cases ported from editfns-tests.el).
!> Each test works in a fresh buffer and restores the previous one.
module test_insdel_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, set_error, FM_BUFFER_READ_ONLY, FM_GENERIC_ERROR
    use lisp_fm, only: value_type, nilp
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, kill_buffer, &
                         set_buffer, current_buffer, buffer_live_p, buf_bytes, &
                         temp_set_point_both, set_buffer_read_only, buf_fetch_char
    use marker_fm, only: char_to_byte, byte_to_char, buf_charpos_to_bytepos
    use insdel_fm, only: del_range, del_range_1, del_range_byte, insert, insert_char, make_gap, &
                         move_gap_both, replace_range, before_change_functions, after_change_functions, &
                         first_change_hook, inhibit_modification_hooks
    use editfns_fm, only: delete_region, delete_and_extract_region, subst_char_in_region, point_min, point_max, &
                          buffer_size, insert_char_n
    use casefiddle_fm, only: upcase_region, capitalize_region
    use textprop_fm, only: put_text_property, get_text_property
    use search_fm, only: search_forward, match_end, replace_match
    implicit none(type, external)
    private

    public :: collect_insdel_fm

    !> "aé€b" etc. as bytes: é = C3 A9, € = E2 82 AC.
    character(len=*), parameter :: e_acute = char(195)//char(169)
    character(len=*), parameter :: euro = char(226)//char(130)//char(172)

    !> What the change functions of the hook tests recorded.
    character(len=:), allocatable :: trace

    !> The state of the sanity checks (editfns-tests.el's
    !> sanity-check-change-functions-* variables).
    integer(int64) :: sc_beg = 0, sc_end = 0, sc_size = 0
    logical :: sc_size_known = .false.
    integer :: sc_errors = 0, sc_calls = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_insdel_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("insert-ascii", test_insert_ascii), &
                    new_unittest("insert-multibyte", test_insert_multibyte), &
                    new_unittest("insert-in-middle", test_insert_middle), &
                    new_unittest("insert-char", test_insert_char), &
                    new_unittest("gap-growth", test_gap_growth), &
                    new_unittest("delete-range", test_delete), &
                    new_unittest("delete-adjusts-point", test_delete_point), &
                    new_unittest("delete-clamps-and-ignores-empty", test_delete_clamp), &
                    new_unittest("delete-by-bytes", test_delete_bytes), &
                    new_unittest("char-byte-conversion", test_conversion), &
                    new_unittest("read-only", test_read_only), &
                    new_unittest("gap-shrink", test_gap_shrink), &
                    new_unittest("position-cache-slot-reuse", test_cache_reuse), &
                    new_unittest("change-hooks-primitives", test_hooks_primitives), &
                    new_unittest("change-hooks-local-and-global", test_hooks_local_and_global), &
                    new_unittest("change-hooks-inhibited", test_hooks_inhibited), &
                    new_unittest("first-change-hook", test_hooks_first_change), &
                    new_unittest("before-change-error-resets", test_hooks_before_error), &
                    new_unittest("after-change-error-resets", test_hooks_after_error), &
                    new_unittest("delete-region-undo-markers-2", test_delete_region_undo_markers_2), &
                    new_unittest("change-hooks-property-retry", test_hooks_property_retry), &
                    new_unittest("change-hooks-replace-match", test_hooks_replace_match), &
                    new_unittest("editfns-tests--before/after-change-functions", &
                                 test_before_after_change_functions) &
                    ]
    end subroutine collect_insdel_fm

    !> Make and select a fresh buffer; remember the old current buffer.
    subroutine enter(name, h, old)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
    end subroutine enter

    subroutine leave(h, old)
        type(handle_type), intent(in) :: h, old
        type(status_type) :: st

        if (buffer_live_p(old)) call set_buffer(old, st)
        call kill_buffer(h, st)
    end subroutine leave

    !> Whole buffer text.
    function contents() result(s)
        character(len=:), allocatable :: s

        s = buf_bytes(buffers(cur), 1_int64, buffers(cur)%text%z_byte)
    end function contents

    !> Move point to char position `pos`.
    subroutine goto(pos)
        integer(int64), intent(in) :: pos

        call temp_set_point_both(buffers(cur), pos, char_to_byte(pos))
    end subroutine goto

    subroutine test_insert_ascii(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-ascii", h, old)
        call insert("hello", st)
        call check(error, st%ok(), "insert failed")
        if (allocated(error)) return
        call check(error, contents(), "hello")
        if (allocated(error)) return
        call check(error, buffers(cur)%pt == 6 .and. buffers(cur)%pt_byte == 6, "point")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z == 6 .and. buffers(cur)%zv == 6, "z/zv")
        if (allocated(error)) return
        call insert("", st)
        call check(error, contents(), "hello")
        call leave(h, old)
    end subroutine test_insert_ascii

    subroutine test_insert_multibyte(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-mb", h, old)
        call insert("a"//e_acute//euro//"b", st)
        call check(error, contents(), "a"//e_acute//euro//"b")
        if (allocated(error)) return
        ! 4 chars, 7 bytes: Z = 5, Z_BYTE = 8.
        call check(error, buffers(cur)%text%z, 5_int64)
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z_byte, 8_int64)
        if (allocated(error)) return
        call check(error, buffers(cur)%pt_byte, 8_int64)
        if (allocated(error)) return
        ! Invalid multibyte text is rejected.
        call insert(char(128), st)
        call check(error,.not. st%ok(), "invalid text accepted")
        call leave(h, old)
    end subroutine test_insert_multibyte

    subroutine test_insert_middle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-mid", h, old)
        call insert("ad", st)
        call goto(2_int64)
        call insert("bc", st)
        call check(error, contents(), "abcd")
        if (allocated(error)) return
        call check(error, buffers(cur)%pt, 4_int64)
        if (allocated(error)) return
        ! Gap left (to 1), then right (to the end).
        call goto(1_int64)
        call insert(euro, st)
        call goto(6_int64)
        call insert(e_acute, st)
        call check(error, contents(), euro//"abcd"//e_acute)
        if (allocated(error)) return
        call goto(3_int64)
        call insert("-", st)
        call check(error, contents(), euro//"a-bcd"//e_acute)
        call leave(h, old)
    end subroutine test_insert_middle

    subroutine test_insert_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-char", h, old)
        call insert_char(int(z'20AC', int32), st)
        call insert_char(int(iachar("x"), int32), st)
        call check(error, contents(), euro//"x")
        if (allocated(error)) return
        call check(error, buf_fetch_char(buffers(cur), 1_int64), int(z'20AC', int32))
        if (allocated(error)) return
        call check(error, buf_fetch_char(buffers(cur), 4_int64), int(iachar("x"), int32))
        call leave(h, old)
    end subroutine test_insert_char

    subroutine test_gap_growth(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer :: k
        character(len=:), allocatable :: s

        call enter("ti-grow", h, old)

        do k = 1, 3000
            call insert("xy", st)
            if (.not. st%ok()) exit
            ! Alternate insertion points to keep moving the gap.
            if (mod(k, 7) == 0) call goto(int(k, int64))
        end do

        call check(error, st%ok(), "insert failed while growing")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z, 6001_int64)
        if (allocated(error)) return
        s = contents()
        call check(error, len(s), 6000)
        if (allocated(error)) return
        call check(error, count_char(s, "x"), 3000)
        if (allocated(error)) return
        call check(error, len(buffers(cur)%text%beg, kind=int64), &
                   buffers(cur)%text%z_byte - 1 + buffers(cur)%text%gap_size)
        call leave(h, old)

    contains

        pure integer function count_char(str, c)
            character(len=*), intent(in) :: str
            character, intent(in) :: c
            integer :: i

            count_char = 0

            do i = 1, len(str)
                if (str(i:i) == c) count_char = count_char + 1
            end do
        end function count_char
    end subroutine test_gap_growth

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: gone

        call enter("ti-del", h, old)
        call insert("a"//e_acute//"bcd"//euro//"e", st)
        call del_range_1(2_int64, 4_int64, .true., st, gone)
        call check(error, gone, e_acute//"b")
        if (allocated(error)) return
        call check(error, contents(), "acd"//euro//"e")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z, 6_int64)
        if (allocated(error)) return
        call del_range(4_int64, 5_int64, st)
        call check(error, contents(), "acde")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z_byte, 5_int64)
        call leave(h, old)
    end subroutine test_delete

    subroutine test_delete_point(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-delpt", h, old)
        call insert("abcdefgh", st)
        ! Point after the range: shifts down.
        call goto(8_int64)
        call del_range(2_int64, 4_int64, st)
        call check(error, buffers(cur)%pt, 6_int64)
        if (allocated(error)) return
        ! Point inside the range: moves to its start.
        call goto(4_int64)
        call del_range(3_int64, 5_int64, st)
        call check(error, buffers(cur)%pt, 3_int64)
        if (allocated(error)) return
        ! Point before the range: unchanged.
        call goto(1_int64)
        call del_range(2_int64, 3_int64, st)
        call check(error, buffers(cur)%pt, 1_int64)
        if (allocated(error)) return
        call check(error, contents(), "agh")
        call leave(h, old)
    end subroutine test_delete_point

    subroutine test_delete_clamp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: tick

        call enter("ti-clamp", h, old)
        call insert("abc", st)
        tick = buffers(cur)%text%modiff
        call del_range(3_int64, 2_int64, st)
        call check(error, contents() == "abc" .and. buffers(cur)%text%modiff == tick, &
                   "reversed range changed the buffer")
        if (allocated(error)) return
        call del_range(-5_int64, 100_int64, st)
        call check(error, contents(), "")
        if (allocated(error)) return
        call check(error, buffers(cur)%pt, 1_int64)
        call leave(h, old)
    end subroutine test_delete_clamp

    subroutine test_delete_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-delb", h, old)
        call insert("x"//euro//"y", st)
        ! Bytes 2..4 are the euro sign.
        call del_range_byte(2_int64, 5_int64, st)
        call check(error, contents(), "xy")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%z, 3_int64)
        call leave(h, old)
    end subroutine test_delete_bytes

    subroutine test_conversion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64), parameter :: bytepos(*) = [1, 2, 4, 7, 8, 9]
        integer(int64) :: c

        call enter("ti-conv", h, old)
        ! chars: a é € b c  -> bytes 1 2 4 7 8 (and Z_BYTE 9)
        call insert("a"//e_acute//euro//"bc", st)
        ! Put the gap in the middle so conversions straddle it.
        call goto(3_int64)
        call insert("", st)
        call move_gap_both(3_int64, 4_int64)

        do c = 1, 6
            call check(error, char_to_byte(c), bytepos(c))
            if (allocated(error)) return
            call check(error, byte_to_char(bytepos(c)), c)
            if (allocated(error)) return
        end do

        ! The far end of the buffer, scanning backwards.
        call goto(1_int64)
        call check(error, buf_charpos_to_bytepos(cur, 5_int64), 8_int64)
        call leave(h, old)
    end subroutine test_conversion

    subroutine test_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-ro", h, old)
        call insert("abc", st)
        call set_buffer_read_only(h, .true., st)
        call insert("x", st)
        call check(error, st%code, FM_BUFFER_READ_ONLY)
        if (allocated(error)) return
        call del_range(1_int64, 2_int64, st)
        call check(error, st%code, FM_BUFFER_READ_ONLY)
        if (allocated(error)) return
        call check(error, contents(), "abc")
        if (allocated(error)) return
        ! Unprepared deletion skips the check, as upstream.
        call del_range_1(1_int64, 2_int64, .false., st)
        call check(error, contents(), "bc")
        call leave(h, old)
    end subroutine test_read_only

    subroutine test_gap_shrink(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-shrink", h, old)
        call insert("abcdef", st)
        call goto(4_int64)
        call make_gap(5000_int64, st)
        call check(error, buffers(cur)%text%gap_size >= 5000, "gap did not grow")
        if (allocated(error)) return
        call make_gap(-100000_int64, st)
        call check(error, buffers(cur)%text%gap_size, 20_int64)
        if (allocated(error)) return
        call check(error, contents(), "abcdef")
        call leave(h, old)
    end subroutine test_gap_shrink

    !> The char/byte cache must not leak from a killed buffer to a new one
    !> that reuses its registry slot with the same modiff.
    subroutine test_cache_reuse(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, keep, old, prev
        type(status_type) :: st
        integer(int64) :: slot, tick

        ! A live, visible buffer to fall back to, so killing A does not
        ! create *scratch* in A's slot.
        call enter("ti-cache-keep", keep, prev)
        call enter("ti-cache-a", a, old)
        call insert(euro//euro//euro//euro//euro//euro, st)
        call check(error, char_to_byte(4_int64), 10_int64)   ! now cached
        if (allocated(error)) return
        slot = a%index
        tick = buffers(cur)%text%modiff
        call leave(a, old)
        call enter("ti-cache-b", b, old)
        call insert("aaaaa"//euro, st)
        call check(error, b%index == slot .and. buffers(cur)%text%modiff == tick, &
                   "test setup: slot or modiff differs")
        if (allocated(error)) return
        call check(error, char_to_byte(4_int64), 4_int64)
        call leave(b, old)
        call leave(keep, prev)
    end subroutine test_cache_reuse

    ! ------------------------------------------------------------------
    ! Change hooks
    ! ------------------------------------------------------------------

    !> The decimal text of N.
    function num(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=21) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function num

    !> Records "B<beg>,<end>"; notes when the hooks are not inhibited.
    subroutine rec_before(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        trace = trace//" B"//num(beg)//","//num(end)
        if (.not. inhibit_modification_hooks%value) trace = trace//"!"
    end subroutine rec_before

    !> Records "A<beg>,<end>,<old_len>".
    subroutine rec_after(beg, end, old_len, status)
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status

        status = status_type()
        trace = trace//" A"//num(beg)//","//num(end)//","//num(old_len)
        if (.not. inhibit_modification_hooks%value) trace = trace//"!"
    end subroutine rec_after

    !> Records "F" (first_change_hook).
    subroutine rec_first(status)
        type(status_type), intent(out) :: status

        status = status_type()
        trace = trace//" F"
    end subroutine rec_first

    !> Records "G<beg>,<end>": a second before-change function.
    subroutine rec_global(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        trace = trace//" G"//num(beg)//","//num(end)
    end subroutine rec_global

    !> A before-change function that fails.
    subroutine fail_before(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        trace = trace//" X"//num(beg)//","//num(end)
        call set_error(status, FM_GENERIC_ERROR, "before failed")
    end subroutine fail_before

    !> An after-change function that fails.
    subroutine fail_after(beg, end, old_len, status)
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status

        trace = trace//" Y"//num(beg)//","//num(end)//","//num(old_len)
        call set_error(status, FM_GENERIC_ERROR, "after failed")
    end subroutine fail_after

    !> editfns-tests.el delete-region-undo-markers-2: deletes one more
    !> character on each side of the text about to change.
    subroutine widen_deletion(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        call delete_region(beg - 1, end + 1, status)
    end subroutine widen_deletion

    !> Inserts "X" at the beginning of the buffer.
    subroutine insert_at_start(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64) :: opoint

        trace = trace//" I"//num(beg)//","//num(end)
        opoint = buffers(cur)%pt
        call goto(1_int64)
        call insert("X", status)
        call goto(opoint + 1)
    end subroutine insert_at_start

    !> Records the end of the last match, as an after-change function.
    subroutine rec_match_end(beg, end, old_len, status)
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status

        trace = trace//" M"//num(match_end(0, status))//" "//num(beg)//","//num(end)//","//num(old_len)
    end subroutine rec_match_end

    !> Give the current buffer the recording functions (buffer-local).
    subroutine record_changes()
        call before_change_functions%add(rec_before, local=.true.)
        call after_change_functions%add(rec_after, local=.true.)
        trace = ""
    end subroutine record_changes

    !> Forget the current buffer's own change functions.
    subroutine forget_changes()
        call before_change_functions%kill_local()
        call after_change_functions%kill_local()
        call first_change_hook%kill_local()
    end subroutine forget_changes

    !> Each primitive runs the before- and after-change functions once,
    !> with the bounds of the change, while inhibiting further hooks.
    subroutine test_hooks_primitives(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: deleted

        call enter("ti-hooks", h, old)
        call record_changes()
        call insert("abc", st)
        call check(error, trace, " B1,1 A1,4,0", "insert")
        if (allocated(error)) return
        trace = ""
        call del_range(2_int64, 3_int64, st)
        call check(error, trace, " B2,3 A2,2,1", "delete")
        if (allocated(error)) return
        trace = ""
        call replace_range(1_int64, 3_int64, "xyz", .true., st)
        call check(error, contents()//trace, "xyz B1,3 A1,4,2", "replace")
        if (allocated(error)) return
        trace = ""
        call replace_range(1_int64, 4_int64, "xyz", .false., st)
        call check(error, trace, "", "replace without hooks")
        if (allocated(error)) return
        trace = ""
        call upcase_region(1_int64, 4_int64, st)
        call check(error, contents()//trace, "XYZ B1,4 A1,4,3", "upcase-region")
        if (allocated(error)) return
        trace = ""
        call upcase_region(1_int64, 4_int64, st)
        call check(error, trace, " B1,4", "upcase-region, nothing changed")
        if (allocated(error)) return
        call goto(4_int64)
        call insert("abab", st)
        trace = ""
        call subst_char_in_region(1_int64, 8_int64, iachar("b", int32), iachar("c", int32), st)
        call check(error, contents()//trace, "XYZacac B5,8 A5,8,3", "subst-char-in-region")
        if (allocated(error)) return
        trace = ""
        call put_text_property(2_int64, 4_int64, sym("face"), sym("bold"), st)
        call check(error, trace, " B2,4 A2,4,2", "put-text-property")
        if (allocated(error)) return
        trace = ""
        call put_text_property(2_int64, 4_int64, sym("face"), sym("bold"), st)
        call check(error, trace, "", "put-text-property, nothing changed")
        if (allocated(error)) return
        trace = ""
        call delete_and_extract_region(1_int64, 2_int64, deleted, st)
        call check(error, deleted//trace, "X B1,2 A1,1,1", "delete-and-extract-region")
        call forget_changes()
        call leave(h, old)
    end subroutine test_hooks_primitives

    !> A buffer's own functions run with the default ones in the place of
    !> `t'; other buffers run only the default ones.
    subroutine test_hooks_local_and_global(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, h2, old2
        type(status_type) :: st

        call before_change_functions%add(rec_global)
        call enter("ti-hooks-local", h, old)
        call record_changes()
        call insert("a", st)
        call check(error, trace, " B1,1 G1,1 A1,2,0", "local, then global")
        if (allocated(error)) return
        call enter("ti-hooks-other", h2, old2)
        trace = ""
        call insert("a", st)
        call check(error, trace, " G1,1", "global only")
        call leave(h2, old2)
        call forget_changes()
        call leave(h, old)
        call before_change_functions%remove(rec_global)
    end subroutine test_hooks_local_and_global

    !> With `inhibit_modification_hooks' no change function runs.
    subroutine test_hooks_inhibited(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-hooks-inhibit", h, old)
        call record_changes()
        call first_change_hook%add(rec_first, local=.true.)
        inhibit_modification_hooks = .true.
        call insert("abc", st)
        call del_range(1_int64, 2_int64, st)
        inhibit_modification_hooks = .false.
        call check(error, contents()//trace, "bc")
        call forget_changes()
        call leave(h, old)
    end subroutine test_hooks_inhibited

    !> `first_change_hook' runs before the first change of an unmodified
    !> buffer, and again once it is unmodified.
    subroutine test_hooks_first_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-hooks-first", h, old)
        call record_changes()
        call first_change_hook%add(rec_first, local=.true.)
        call insert("a", st)
        call insert("b", st)
        call check(error, trace, " F B1,1 A1,2,0 B2,2 A2,3,0")
        if (allocated(error)) return
        buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
        trace = ""
        call del_range(1_int64, 2_int64, st)
        call check(error, trace, " F B1,2 A1,1,1")
        call forget_changes()
        call leave(h, old)
    end subroutine test_hooks_first_change

    !> An error in a before-change function: the change is not made, the
    !> error is returned and the hook is emptied in the current buffer,
    !> which then runs no default functions either.  Adding a function
    !> to the default value then adds it to that emptied value (subr.el's
    !> "make-local-variable was used on a hook").
    subroutine test_hooks_before_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call before_change_functions%add(rec_global)
        call enter("ti-hooks-error", h, old)
        call insert("abc", st)
        call before_change_functions%add(fail_before, local=.true.)
        trace = ""
        call insert("x", st)
        call check(error, st%code, FM_GENERIC_ERROR, "error returned")
        if (allocated(error)) return
        call check(error, contents()//trace, "abc X4,4", "nothing inserted")
        if (allocated(error)) return
        call check(error, before_change_functions%local_p() .and. before_change_functions%empty(), &
                   "emptied locally")
        if (allocated(error)) return
        trace = ""
        call insert("x", st)
        call check(error, contents()//trace, "abcx", "no functions run")
        if (allocated(error)) return
        call before_change_functions%add(rec_before)
        trace = ""
        call insert("y", st)
        call check(error, trace, " B5,5", "added to the buffer's value")
        if (allocated(error)) return
        call before_change_functions%remove(rec_before)
        call forget_changes()
        call check(error, before_change_functions%member(rec_global), "default value kept")
        call leave(h, old)
        call before_change_functions%remove(rec_global)
    end subroutine test_hooks_before_error

    !> An error in an after-change function: the change stays, the error
    !> is returned and the default value is emptied (no buffer-local
    !> value).
    subroutine test_hooks_after_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-hooks-after-error", h, old)
        call after_change_functions%add(fail_after)
        trace = ""
        call insert("abc", st)
        call check(error, st%code, FM_GENERIC_ERROR, "error returned")
        if (allocated(error)) return
        call check(error, contents()//trace, "abc Y1,4,0")
        if (allocated(error)) return
        call check(error, after_change_functions%empty() .and. .not. after_change_functions%local_p(), &
                   "default value emptied")
        if (allocated(error)) return
        call check(error,.not. inhibit_modification_hooks%value, "binding undone")
        call leave(h, old)
    end subroutine test_hooks_after_error

    !> editfns-tests.el: delete-region-undo-markers-2.  The before-change
    !> function's deletion moves the start of the deletion (kept in a
    !> marker) and the rest keeps its length.  (The marker adjustments
    !> upstream records in the undo list are deferred.)
    subroutine test_delete_region_undo_markers_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("ti-undo-markers-2", h, old)
        call insert("1234567890", st)
        call before_change_functions%add(widen_deletion, local=.true.)
        call delete_region(2_int64, 5_int64, st)
        call check(error, st%ok(), "delete-region")
        if (allocated(error)) return
        call check(error, contents(), "90")
        call forget_changes()
        call leave(h, old)
    end subroutine test_delete_region_undo_markers_2

    !> A before-change function that changes the buffer while text
    !> properties are being put: the intervals are looked at again.
    subroutine test_hooks_property_retry(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: v

        call enter("ti-hooks-props", h, old)
        call insert("abcd", st)
        call before_change_functions%add(insert_at_start, local=.true.)
        call after_change_functions%add(rec_after, local=.true.)
        trace = ""
        call put_text_property(2_int64, 4_int64, sym("face"), sym("bold"), st)
        call check(error, contents()//trace, "Xabcd I2,4 A2,4,2")
        if (allocated(error)) return
        call get_text_property(2_int64, sym("face"), v, st)
        call check(error, value_eq(v, sym("bold")), "property at 2")
        if (allocated(error)) return
        call get_text_property(4_int64, sym("face"), v, st)
        call check(error, nilp(v), "no property at 4")
        call forget_changes()
        call leave(h, old)
    end subroutine test_hooks_property_retry

    !> replace-match adjusts the match data before the after-change
    !> functions run (replace_range's ADJUST_MATCH_DATA).
    subroutine test_hooks_replace_match(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: found

        call enter("ti-hooks-replace-match", h, old)
        call insert("abc", st)
        call goto(1_int64)
        call search_forward("b", found, st)
        call after_change_functions%add(rec_match_end, local=.true.)
        trace = ""
        call replace_match("XYZ", st, fixedcase=.true., literal=.true.)
        call check(error, contents()//trace, "aXYZc M5 2,5,1")
        call forget_changes()
        call leave(h, old)
    end subroutine test_hooks_replace_match

    ! editfns-tests.el: the sanity checks of the before/after change
    ! functions.
    !> sanity-check-change-functions-check-size.
    subroutine sanity_check_size()
        if (.not. sc_size_known) then
            sc_size = buffer_size()
            sc_size_known = .true.
        else if (sc_size /= buffer_size()) then
            sc_errors = sc_errors + 1
            sc_size = buffer_size()
        end if
    end subroutine sanity_check_size

    !> sanity-check-change-functions-before.
    subroutine sanity_before(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. (point_min() <= beg .and. beg <= end .and. end <= point_max())) sc_errors = sc_errors + 1
        call sanity_check_size()
        sc_calls = sc_calls + 1
        sc_beg = beg
        sc_end = end
    end subroutine sanity_before

    !> sanity-check-change-functions-after.
    subroutine sanity_after(beg, end, len, status)
        integer(int64), intent(in) :: beg, end, len
        type(status_type), intent(out) :: status
        integer(int64) :: bend, offset

        status = status_type()
        if (.not. (point_min() <= beg .and. beg <= end .and. end <= point_max())) sc_errors = sc_errors + 1
        if (len < 0) sc_errors = sc_errors + 1
        bend = beg + len
        if (.not. (sc_beg <= beg .and. beg <= bend .and. bend <= sc_end)) sc_errors = sc_errors + 1
        offset = end - beg - len
        sc_end = sc_end + offset
        sc_size = sc_size + offset
        call sanity_check_size()
    end subroutine sanity_after

    !> editfns-tests.el: editfns-tests--before/after-change-functions, with
    !> the operations that are ported (dabbrev and the coding functions
    !> are not) in their place.
    subroutine test_before_after_change_functions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: beg, found

        call enter("ti-sanity", h, old)
        call before_change_functions%add(sanity_before, local=.true.)
        call after_change_functions%add(sanity_after, local=.true.)
        sc_errors = 0
        sc_calls = 0
        sc_size_known = .false.
        call insert("utf-8-unix"//char(10)//char(10)//"UTF", st)
        beg = buffers(cur)%pt
        call insert(e_acute//e_acute//e_acute, st)
        call upcase_region(beg, buffers(cur)%pt, st)
        call capitalize_region(1_int64, buffers(cur)%pt, st)
        call subst_char_in_region(1_int64, buffers(cur)%pt, iachar("-", int32), iachar("_", int32), st)
        call put_text_property(2_int64, 6_int64, sym("face"), sym("bold"), st)
        call delete_region(3_int64, 8_int64, st)
        call goto(1_int64)
        call search_forward("x", found, st)
        call replace_match("XYZW", st, fixedcase=.true., literal=.true.)
        call goto(buffers(cur)%zv)
        call insert_char_n(233_int32, 3_int64, st)
        call sanity_check_size()
        call check(error, sc_calls, 9, "before-change calls")
        if (allocated(error)) return
        call check(error, sc_errors, 0)
        call forget_changes()
        call leave(h, old)
    end subroutine test_before_after_change_functions
end module test_insdel_fm
