! test_textprop_fm.f90 -- tests for intervals_fm and textprop_fm.
! Includes a case ported from GNU Emacs 31.1 test/src/textprop-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

module test_textprop_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_TEXT_READ_ONLY, FM_BUFFER_READ_ONLY, &
                        FM_ARGS_OUT_OF_RANGE, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: list, make_fixnum, make_string, nil, nilp, t_value, value_type
    use lread_fm, only: sym
    use data_fm, only: value_eq, set
    use fns_fm, only: value_equal, put
    use buffer_fm, only: buffers, cur, get_buffer_create, kill_buffer, set_buffer, &
                         current_buffer, buffer_live_p, set_buffer_read_only, &
                         erase_buffer, inhibit_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, delete_and_extract_region, point_max
    use textprop_fm, only: add_text_properties, get_text_property, next_property_change, &
                           next_single_property_change, previous_property_change, &
                           previous_single_property_change, put_text_property, &
                           remove_list_of_text_properties, remove_text_properties, &
                           set_text_properties, text_properties_at, text_property_any, &
                           text_property_not_all
    implicit none(type, external)
    private

    public :: collect_textprop_fm

contains

    subroutine collect_textprop_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("put-and-get", test_put_get), &
                    new_unittest("add-reports-change", test_add_change), &
                    new_unittest("set-and-remove", test_set_remove), &
                    new_unittest("property-changes", test_changes), &
                    new_unittest("property-any-not-all", test_any_not_all), &
                    new_unittest("category-fallback", test_category), &
                    new_unittest("edits-move-properties", test_edits), &
                    new_unittest("property-change-modifies", test_modifies), &
                    new_unittest("odd-plist-and-range-errors", test_errors), &
                    new_unittest("read-only-insertion", test_read_only_insert), &
                    new_unittest("textprop-interval-immutability", test_immutability) &
                    ]
    end subroutine collect_textprop_fm

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

    function prop_at(pos, name) result(v)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st
        call get_text_property(pos, sym(name), v, st)
    end function prop_at

    subroutine test_put_get(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: plist
        call enter("tt-put", h, old)
        call insert("abcdefgh", st)
        call put_text_property(3_int64, 6_int64, sym("face"), sym("bold"), st)
        call check(error, st%ok(), "put failed")
        if (allocated(error)) return
        call check(error, nilp(prop_at(2_int64, "face")), "before range")
        if (allocated(error)) return
        call check(error, value_eq(prop_at(3_int64, "face"), sym("bold")), "start of range")
        if (allocated(error)) return
        call check(error, value_eq(prop_at(5_int64, "face"), sym("bold")), "end of range")
        if (allocated(error)) return
        call check(error, nilp(prop_at(6_int64, "face")), "after range")
        if (allocated(error)) return
        ! A second property goes in front, as upstream.
        call put_text_property(4_int64, 5_int64, sym("help-echo"), make_string("hi"), st)
        call text_properties_at(4_int64, plist, st)
        call check(error, value_equal(plist, list(sym("help-echo"), make_string("hi"), sym("face"), sym("bold"))), &
                   "text-properties-at order")
        if (allocated(error)) return
        call text_properties_at(9_int64, plist, st)
        call check(error, st%ok() .and. nilp(plist), "at point-max")
        call leave(h, old)
    end subroutine test_put_get

    subroutine test_add_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed
        call enter("tt-add", h, old)
        call insert("abcdef", st)
        call add_text_properties(1_int64, 4_int64, list(sym("a"), make_fixnum(1_int64)), changed, st)
        call check(error, changed, "first add did not change")
        if (allocated(error)) return
        call add_text_properties(2_int64, 3_int64, list(sym("a"), make_fixnum(1_int64)), changed, st)
        call check(error,.not. changed, "re-adding the same value changed")
        if (allocated(error)) return
        call add_text_properties(2_int64, 6_int64, list(sym("a"), make_fixnum(1_int64)), changed, st)
        call check(error, changed .and. value_eq(prop_at(5_int64, "a"), make_fixnum(1_int64)), "extending")
        if (allocated(error)) return
        call add_text_properties(3_int64, 3_int64, list(sym("b"), make_fixnum(1_int64)), changed, st)
        call check(error,.not. changed, "empty range changed")
        call leave(h, old)
    end subroutine test_add_change

    subroutine test_set_remove(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed
        type(value_type) :: plist
        call enter("tt-set", h, old)
        call insert("abcdefgh", st)
        call put_text_property(1_int64, 9_int64, sym("face"), sym("bold"), st)
        call put_text_property(1_int64, 9_int64, sym("x"), make_fixnum(1_int64), st)
        call remove_text_properties(3_int64, 5_int64, list(sym("face"), nil()), changed, st)
        call check(error, changed .and. nilp(prop_at(3_int64, "face")) .and. &
                   value_eq(prop_at(3_int64, "x"), make_fixnum(1_int64)) .and. &
                   value_eq(prop_at(5_int64, "face"), sym("bold")), "remove-text-properties")
        if (allocated(error)) return
        call remove_text_properties(3_int64, 5_int64, list(sym("face"), nil()), changed, st)
        call check(error,.not. changed, "removing again changed")
        if (allocated(error)) return
        call remove_list_of_text_properties(1_int64, 9_int64, list(sym("x")), changed, st)
        call check(error, changed .and. nilp(prop_at(7_int64, "x")), "remove-list-of-text-properties")
        if (allocated(error)) return
        call set_text_properties(2_int64, 4_int64, list(sym("y"), make_fixnum(2_int64)), changed, st)
        call text_properties_at(2_int64, plist, st)
        call check(error, value_equal(plist, list(sym("y"), make_fixnum(2_int64))), "set replaces all")
        if (allocated(error)) return
        call set_text_properties(1_int64, 9_int64, nil(), changed, st)
        call check(error, changed .and. nilp(prop_at(2_int64, "y")) .and. nilp(prop_at(6_int64, "face")), &
                   "set to nil clears")
        call leave(h, old)
    end subroutine test_set_remove

    subroutine test_changes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: r
        logical :: found
        ! a b c d e f g h i j:  [3,5) face bold, [5,7) face bold + x 1
        call enter("tt-changes", h, old)
        call insert("abcdefghij", st)
        call put_text_property(3_int64, 7_int64, sym("face"), sym("bold"), st)
        call put_text_property(5_int64, 7_int64, sym("x"), make_fixnum(1_int64), st)
        call next_property_change(1_int64, r, found, st)
        call check(error, found .and. r == 3, "next from 1")
        if (allocated(error)) return
        call next_property_change(3_int64, r, found, st)
        call check(error, found .and. r == 5, "next from 3")
        if (allocated(error)) return
        call next_single_property_change(3_int64, sym("face"), r, found, st)
        call check(error, found .and. r == 7, "next single face from 3")
        if (allocated(error)) return
        call next_property_change(7_int64, r, found, st)
        call check(error,.not. found, "nothing after the last change")
        if (allocated(error)) return
        call next_property_change(7_int64, r, found, st, limit=9_int64)
        call check(error, found .and. r == 9, "limit returned")
        if (allocated(error)) return
        call next_property_change(1_int64, r, found, st, limit=2_int64)
        call check(error, found .and. r == 2, "limit before change")
        if (allocated(error)) return
        call previous_property_change(11_int64, r, found, st)
        call check(error, found .and. r == 7, "previous from end")
        if (allocated(error)) return
        call previous_single_property_change(7_int64, sym("face"), r, found, st)
        call check(error, found .and. r == 3, "previous single face from 7")
        if (allocated(error)) return
        call previous_property_change(3_int64, r, found, st)
        call check(error,.not. found, "nothing before the first change")
        call leave(h, old)
    end subroutine test_changes

    subroutine test_any_not_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: r
        logical :: found
        call enter("tt-any", h, old)
        call insert("abcdefghij", st)
        call text_property_any(1_int64, 11_int64, sym("face"), nil(), r, found, st)
        call check(error, found .and. r == 1, "no intervals: nil everywhere")
        if (allocated(error)) return
        call put_text_property(4_int64, 6_int64, sym("face"), sym("bold"), st)
        call text_property_any(1_int64, 11_int64, sym("face"), sym("bold"), r, found, st)
        call check(error, found .and. r == 4, "any bold")
        if (allocated(error)) return
        call text_property_any(6_int64, 11_int64, sym("face"), sym("bold"), r, found, st)
        call check(error,.not. found, "no bold after 6")
        if (allocated(error)) return
        call text_property_not_all(4_int64, 11_int64, sym("face"), sym("bold"), r, found, st)
        call check(error, found .and. r == 6, "not-all")
        if (allocated(error)) return
        call text_property_not_all(4_int64, 6_int64, sym("face"), sym("bold"), r, found, st)
        call check(error,.not. found, "all bold in 4..6")
        call leave(h, old)
    end subroutine test_any_not_all

    subroutine test_category(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        call enter("tt-cat", h, old)
        call insert("abc", st)
        call put(sym("tt-my-category"), sym("face"), sym("italic"), st)
        call put_text_property(1_int64, 3_int64, sym("category"), sym("tt-my-category"), st)
        call check(error, value_eq(prop_at(1_int64, "face"), sym("italic")), "face via category")
        if (allocated(error)) return
        call put_text_property(1_int64, 2_int64, sym("face"), sym("bold"), st)
        call check(error, value_eq(prop_at(1_int64, "face"), sym("bold")), "own property wins")
        call leave(h, old)
    end subroutine test_category

    !> Properties follow their text through insertions and deletions;
    !> inserted text has none.
    subroutine test_edits(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: s
        call enter("tt-edits", h, old)
        call insert("abcdef", st)
        call put_text_property(3_int64, 5_int64, sym("face"), sym("bold"), st)   ! "cd"
        call goto_char(4_int64)
        call insert("XY", st)                                                     ! ab c XY d ef
        call check(error, value_eq(prop_at(3_int64, "face"), sym("bold")) .and. &
                   nilp(prop_at(4_int64, "face")) .and. nilp(prop_at(5_int64, "face")) .and. &
                   value_eq(prop_at(6_int64, "face"), sym("bold")) .and. nilp(prop_at(7_int64, "face")), &
                   "after insertion inside")
        if (allocated(error)) return
        call goto_char(1_int64)
        call insert("Z", st)                                                      ! Z ab c XY d ef
        call check(error, value_eq(prop_at(4_int64, "face"), sym("bold")), "shifted by insertion before")
        if (allocated(error)) return
        call delete_and_extract_region(1_int64, 5_int64, s, st)                  ! XY d ef
        call check(error, nilp(prop_at(1_int64, "face")) .and. value_eq(prop_at(3_int64, "face"), sym("bold")) .and. &
                   nilp(prop_at(4_int64, "face")), "after deletion")
        if (allocated(error)) return
        call delete_and_extract_region(1_int64, point_max(), s, st)
        call check(error, buffers(cur)%text%intervals%n == 0, "all text gone: no intervals")
        call leave(h, old)
    end subroutine test_edits

    subroutine test_modifies(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: tick, chars_tick
        call enter("tt-mod", h, old)
        call insert("abc", st)
        tick = buffers(cur)%text%modiff
        chars_tick = buffers(cur)%text%chars_modiff
        call put_text_property(1_int64, 2_int64, sym("face"), sym("bold"), st)
        call check(error, buffers(cur)%text%modiff > tick, "modiff did not advance")
        if (allocated(error)) return
        call check(error, buffers(cur)%text%chars_modiff, chars_tick)
        call leave(h, old)
    end subroutine test_modifies

    subroutine test_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed
        call enter("tt-errors", h, old)
        call insert("abc", st)
        call add_text_properties(1_int64, 2_int64, list(sym("a"), sym("b"), sym("c")), changed, st)
        call check(error, st%code == FM_GENERIC_ERROR .and. .not. changed, "odd plist")
        if (allocated(error)) return
        call put_text_property(0_int64, 2_int64, sym("a"), sym("b"), st)
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        call leave(h, old)
    end subroutine test_errors

    !> Insertion next to read-only text obeys stickiness: read-only is
    !> rear-sticky by default, so inserting after it fails, before it works.
    subroutine test_read_only_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        call enter("tt-ro-ins", h, old)
        call insert("abcdef", st)
        call put_text_property(3_int64, 5_int64, sym("read-only"), t_value(), st)
        call goto_char(5_int64)
        call insert("x", st)
        call check(error, st%code, FM_TEXT_READ_ONLY)
        if (allocated(error)) return
        call goto_char(3_int64)
        call insert("x", st)
        call check(error, st%ok(), "insertion before read-only text refused")
        if (allocated(error)) return
        call goto_char(5_int64)
        call insert("x", st)
        call check(error, st%code, FM_TEXT_READ_ONLY)
        if (allocated(error)) return
        call put_text_property(4_int64, 6_int64, sym("rear-nonsticky"), t_value(), st)
        call check(error, st%code, FM_TEXT_READ_ONLY)   ! changing read-only text
        if (allocated(error)) return
        call set(sym("inhibit-read-only"), t_value(), st)
        call put_text_property(4_int64, 6_int64, sym("rear-nonsticky"), t_value(), st)
        call set(sym("inhibit-read-only"), nil(), st)
        call goto_char(6_int64)
        call insert("y", st)
        call check(error, st%ok(), "rear-nonsticky read-only still refused insertion")
        call leave(h, old)
    end subroutine test_read_only_insert

    !> textprop-tests.el: textprop-interval-immutability.  `propertize`
    !> strings are replaced by inserting text and putting properties on it.
    subroutine test_immutability(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: s
        call enter("tt-immutable", h, old)
        run: block
            call fill(st)
            call check(error, st%ok(), "building the template failed")
            if (allocated(error)) exit run
            call set_buffer_read_only(h, .true., st)
            ! Delete an entire inhibit-read-only region.
            call delete_and_extract_region(1_int64, 6_int64, s, st)
            call check(error, st%ok() .and. s == "12345", "delete 1-6")
            if (allocated(error)) exit run
            call refill()
            ! Delete multiple characters inside an inhibit-read-only section.
            call delete_and_extract_region(2_int64, 5_int64, s, st)
            call check(error, st%ok() .and. s == "234", "delete 2-5")
            if (allocated(error)) exit run
            call refill()
            ! Across an inhibit-read-only and a read-only region.
            call set_buffer_read_only(h, .false., st)
            call delete_and_extract_region(4_int64, 7_int64, s, st)
            call check(error,.not. st%ok(), "delete 4-7 should fail")
            if (allocated(error)) exit run
            call set(sym("inhibit-read-only"), list(sym("abcdefg")), st)
            ! The same, with the second section's read-only suppressed.
            call delete_and_extract_region(4_int64, 7_int64, s, st)
            call check(error, st%ok() .and. s == "456", "delete 4-7 with abcdefg suppressed")
            if (allocated(error)) exit run
            call refill()
            call set_buffer_read_only(h, .true., st)
            ! Across the suppressed region and two inhibit-read-only regions.
            call delete_and_extract_region(7_int64, 17_int64, s, st)
            call check(error, st%ok() .and. s == "7890ABCDEF", "delete 7-17")
            if (allocated(error)) exit run
            call refill()
            call set(sym("inhibit-read-only"), nil(), st)
            ! Two inhibit-read-only sections separated by immutable text.
            call delete_and_extract_region(17_int64, 27_int64, s, st)
            call check(error, st%code, FM_BUFFER_READ_ONLY)
            if (allocated(error)) exit run
            call set(sym("inhibit-read-only"), list(sym("abcdefg")), st)
            ! From an inhibit-read-only section into protected text.
            call delete_and_extract_region(26_int64, 37_int64, s, st)
            call check(error, st%code, FM_TEXT_READ_ONLY)
        end block run
        call set(sym("inhibit-read-only"), nil(), st)
        call leave(h, old)
    contains

        subroutine fill(st)
            type(status_type), intent(out) :: st
            call insert("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ", st)
            call put_text_property(1_int64, 6_int64, sym("inhibit-read-only"), t_value(), st)
            call put_text_property(6_int64, 11_int64, sym("read-only"), sym("abcdefg"), st)
            call put_text_property(11_int64, 16_int64, sym("inhibit-read-only"), t_value(), st)
            call put_text_property(16_int64, 21_int64, sym("inhibit-read-only"), sym("yes"), st)
            call put_text_property(26_int64, 31_int64, sym("inhibit-read-only"), t_value(), st)
            call put_text_property(31_int64, 37_int64, sym("read-only"), sym("not-suppressed"), st)
        end subroutine fill

        !> (let ((inhibit-read-only t)) (erase-buffer) (insert template))
        subroutine refill()
            type(value_type) :: saved
            type(status_type) :: st2
            saved = inhibit_read_only()
            call set(sym("inhibit-read-only"), t_value(), st2)
            call erase_buffer(st2)
            call fill(st2)
            call set(sym("inhibit-read-only"), saved, st2)
        end subroutine refill
    end subroutine test_immutability
end module test_textprop_fm
