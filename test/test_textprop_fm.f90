! test_textprop_fm.f90 -- tests for intervals_fm and textprop_fm.
! Ports GNU Emacs 31.1 test/src/textprop-tests.el in part;
! textprop-tests-format needs `format' and the other test needs
! font-lock--remove-face-from-text-property.
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
    use lisp_fm, only: list, make_fixnum, make_string, nil, nilp, t_value, value_type, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq, set
    use fns_fm, only: value_equal, put
    use buffer_fm, only: buffers, cur, get_buffer_create, kill_buffer, set_buffer, &
                         current_buffer, is_buffer_live, set_buffer_read_only, &
                         erase_buffer, inhibit_read_only, deactivate_mark_variable, UE_FIRST_CHANGE
    use undo_fm, only: undo_last_kind
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, delete_and_extract_region, point_max
    use textprop_fm, only: string_put_text_property, string_get_text_property, string_next_single_property_change, &
                           propertized_substring, propertized_concat, propertize
    use textprop_fm, only: add_text_properties, get_text_property, next_property_change, &
                           next_single_property_change, previous_property_change, &
                           previous_single_property_change, put_text_property, &
                           remove_list_of_text_properties, remove_text_properties, &
                           set_text_properties, text_properties_at, text_property_any, &
                           text_property_not_all, text_property_stickiness, text_property_range_type, &
                           text_property_list, extend_property_ranges, copy_text_properties, inhibit_point_motion_hooks
    use eval_fm, only: property_function_type
    use keymap_fm, only: make_sparse_keymap, free_keymap, get_local_map
    use intervals_fm, only: copy_intervals_to_string, compare_string_intervals
    implicit none(type, external)
    private

    public :: collect_textprop_fm

    !> What the property functions recorded.
    character(len=:), allocatable :: tp_trace
    type(property_function_type), target :: tp_mod_fn, tp_behind_fn, tp_front_fn, tp_entered_fn, tp_left_fn

contains

    !> Collect all exported unit tests.
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
                    new_unittest("textprop-interval-immutability", test_immutability), &
                    new_unittest("default-nonsticky", test_default_nonsticky), &
                    new_unittest("string-properties", test_string_properties), &
                    new_unittest("text-property-modification-hooks", test_modification_hooks), &
                    new_unittest("intangible-and-point-motion-hooks", test_point_motion), &
                    new_unittest("text-property-lists", test_property_lists), &
                    new_unittest("keymap-and-local-map-properties", test_local_map) &
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

        if (is_buffer_live(old)) call set_buffer(old, st)
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
        if (allocated(error)) return
        ! As textprop.c:modify_text_properties: the first change of an
        ! unmodified buffer is recorded for undo, and the mark deactivated
        ! (prepare_to_modify_buffer_1).
        buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
        deactivate_mark_variable = .false.
        call put_text_property(2_int64, 3_int64, sym("face"), sym("bold"), st)
        call check(error, undo_last_kind() == UE_FIRST_CHANGE, "first change not recorded")
        if (allocated(error)) return
        call check(error, deactivate_mark_variable, "deactivate-mark not set")
        deactivate_mark_variable = .false.
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
        call put_text_property(3_int64, 5_int64, sym("read_only"), t_value(), st)
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
        call put_text_property(4_int64, 6_int64, sym("rear_nonsticky"), t_value(), st)
        call check(error, st%code, FM_TEXT_READ_ONLY)   ! changing read-only text
        if (allocated(error)) return
        call set(sym("inhibit_read_only"), t_value(), st)
        call put_text_property(4_int64, 6_int64, sym("rear_nonsticky"), t_value(), st)
        call set(sym("inhibit_read_only"), nil(), st)
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
            call set(sym("inhibit_read_only"), list(sym("abcdefg")), st)
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
            call set(sym("inhibit_read_only"), nil(), st)
            ! Two inhibit-read-only sections separated by immutable text.
            call delete_and_extract_region(17_int64, 27_int64, s, st)
            call check(error, st%code, FM_BUFFER_READ_ONLY)
            if (allocated(error)) exit run
            call set(sym("inhibit_read_only"), list(sym("abcdefg")), st)
            ! From an inhibit-read-only section into protected text.
            call delete_and_extract_region(26_int64, 37_int64, s, st)
            call check(error, st%code, FM_TEXT_READ_ONLY)
        end block run

        call set(sym("inhibit_read_only"), nil(), st)
        call leave(h, old)

    contains

        subroutine fill(st)
            type(status_type), intent(out) :: st

            call insert("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ", st)
            call put_text_property(1_int64, 6_int64, sym("inhibit_read_only"), t_value(), st)
            call put_text_property(6_int64, 11_int64, sym("read_only"), sym("abcdefg"), st)
            call put_text_property(11_int64, 16_int64, sym("inhibit_read_only"), t_value(), st)
            call put_text_property(16_int64, 21_int64, sym("inhibit_read_only"), sym("yes"), st)
            call put_text_property(26_int64, 31_int64, sym("inhibit_read_only"), t_value(), st)
            call put_text_property(31_int64, 37_int64, sym("read_only"), sym("not-suppressed"), st)
        end subroutine fill

        !> (let ((inhibit-read-only t)) (erase-buffer) (insert template))
        subroutine refill()
            type(value_type) :: saved
            type(status_type) :: st2

            saved = inhibit_read_only()
            call set(sym("inhibit_read_only"), t_value(), st2)
            call erase_buffer(st2)
            call fill(st2)
            call set(sym("inhibit_read_only"), saved, st2)
        end subroutine refill
    end subroutine test_immutability

    !> The standard text-property-default-nonsticky makes syntax-table,
    !> display, composition and fill-space rear-nonsticky; other properties
    !> such as `cursor' are rear-sticky.
    subroutine test_default_nonsticky(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tt-nonsticky", h, old)
        call insert("abc", st)
        call put_text_property(1_int64, 2_int64, sym("cursor"), t_value(), st)
        call put_text_property(1_int64, 2_int64, sym("syntax_table"), t_value(), st)
        call put_text_property(1_int64, 2_int64, sym("fill_space"), t_value(), st)
        call check(error, text_property_stickiness(sym("cursor"), 2_int64) == -1, "cursor is rear-sticky")
        if (allocated(error)) return
        call check(error, text_property_stickiness(sym("syntax_table"), 2_int64) == 0, "syntax_table")
        if (allocated(error)) return
        call check(error, text_property_stickiness(sym("fill_space"), 2_int64) == 0, "fill_space")
        call leave(h, old)
    end subroutine test_default_nonsticky

    !> Text properties of strings: put, get, propertize, substring,
    !> concatenation and the next change; `equal' ignores them.
    subroutine test_string_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: s, part, both
        type(status_type) :: st
        integer(int64) :: pos
        logical :: found

        s = make_string("abcdef")
        call string_put_text_property(1_int64, 3_int64, sym("face"), sym("bold"), s, st)
        call check(error, st%ok(), "put")
        if (allocated(error)) return
        call check(error, nilp(string_get_text_property(0_int64, sym("face"), s)) .and. &
                   value_eq(string_get_text_property(1_int64, sym("face"), s), sym("bold")) .and. &
                   value_eq(string_get_text_property(2_int64, sym("face"), s), sym("bold")) .and. &
                   nilp(string_get_text_property(3_int64, sym("face"), s)), "face on bc")
        if (allocated(error)) return
        call check(error, value_equal(s, make_string("abcdef")), "equal ignores properties")
        if (allocated(error)) return
        call string_next_single_property_change(0_int64, sym("face"), s, pos, found)
        call check(error, found .and. pos == 1, "next change at 1")
        if (allocated(error)) return
        call string_next_single_property_change(1_int64, sym("face"), s, pos, found)
        call check(error, found .and. pos == 3, "next change at 3")
        if (allocated(error)) return
        call string_next_single_property_change(3_int64, sym("face"), s, pos, found)
        call check(error,.not. found, "no change after 3")
        if (allocated(error)) return
        call string_put_text_property(0_int64, 7_int64, sym("face"), sym("bold"), s, st)
        call check(error,.not. st%ok(), "args-out-of-range")
        if (allocated(error)) return
        part = propertized_substring(s, 2_int64, 5_int64)
        call check(error, value_equal(part, make_string("cde")) .and. &
                   value_eq(string_get_text_property(0_int64, sym("face"), part), sym("bold")) .and. &
                   nilp(string_get_text_property(1_int64, sym("face"), part)), "substring keeps c's face")
        if (allocated(error)) return
        both = propertized_concat(part, propertize(make_string("xy"), list(sym("cursor"), make_fixnum(1_int64))))
        call check(error, value_equal(both, make_string("cdexy")) .and. &
                   value_eq(string_get_text_property(0_int64, sym("face"), both), sym("bold")) .and. &
                   nilp(string_get_text_property(3_int64, sym("face"), both)) .and. &
                   value_eq(string_get_text_property(4_int64, sym("cursor"), both), make_fixnum(1_int64)), &
                   "concat keeps both")
    end subroutine test_string_properties

    ! ------------------------------------------------------------------
    ! Modification hooks, point motion, property lists, keymaps
    ! ------------------------------------------------------------------

    ! Property functions recording their calls in `tp_trace'.
    subroutine tp_rec_mod(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" M"//num(beg)//","//num(end)
    end subroutine tp_rec_mod

    ! See `tp_rec_mod'.
    subroutine tp_rec_behind(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" B"//num(beg)//","//num(end)
    end subroutine tp_rec_behind

    ! See `tp_rec_mod'.
    subroutine tp_rec_front(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" F"//num(beg)//","//num(end)
    end subroutine tp_rec_front

    ! See `tp_rec_mod'.
    subroutine tp_rec_entered(old_point, new_point, status)
        integer(int64), intent(in) :: old_point, new_point
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" E"//num(old_point)//","//num(new_point)
    end subroutine tp_rec_entered

    ! See `tp_rec_mod'.
    subroutine tp_rec_left(old_point, new_point, status)
        integer(int64), intent(in) :: old_point, new_point
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" L"//num(old_point)//","//num(new_point)
    end subroutine tp_rec_left

    function num(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function num

    !> Define the recording property functions.  Not a port.
    subroutine define_tp_functions()
        tp_mod_fn = property_function_type("tp_rec_mod", positions=tp_rec_mod)
        call tp_mod_fn%define()
        tp_behind_fn = property_function_type("tp_rec_behind", positions=tp_rec_behind)
        call tp_behind_fn%define()
        tp_front_fn = property_function_type("tp_rec_front", positions=tp_rec_front)
        call tp_front_fn%define()
        tp_entered_fn = property_function_type("tp_rec_entered", positions=tp_rec_entered)
        call tp_entered_fn%define()
        tp_left_fn = property_function_type("tp_rec_left", positions=tp_rec_left)
        call tp_left_fn%define()
    end subroutine define_tp_functions

    !> The `modification_hooks' of changed text run before the change
    !> with its bounds; after an insertion the `insert_behind_hooks' of the
    !> text before it and the `insert_in_front_hooks' of the text after it
    !> run with the bounds of the new text.  Own test.
    subroutine test_modification_hooks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: text

        call define_tp_functions()
        call enter("*tp-hooks*", h, old)
        call insert("abcd", st)
        call put_text_property(2_int64, 4_int64, sym("modification_hooks"), list(sym("tp_rec_mod")), st)
        call put_text_property(1_int64, 2_int64, sym("insert_behind_hooks"), list(sym("tp_rec_behind")), st)
        call put_text_property(4_int64, 5_int64, sym("insert_in_front_hooks"), list(sym("tp_rec_front")), st)
        tp_trace = ""
        call delete_and_extract_region(2_int64, 3_int64, text, st)
        call check(error, tp_trace == " M2,3", "modification hook: '"//tp_trace//"'")
        if (allocated(error)) return
        ! "acd": insert after "a" (behind hooks), then before "d".
        tp_trace = ""
        call goto_char(2_int64)
        call insert("x", st)
        call check(error, tp_trace == " B2,3", "insert behind: '"//tp_trace//"'")
        if (allocated(error)) return
        tp_trace = ""
        call goto_char(4_int64)
        call insert("y", st)
        call check(error, tp_trace == " F4,5", "insert in front: '"//tp_trace//"'")
        call leave(h, old)
    end subroutine test_modification_hooks

    !> With `inhibit_point_motion_hooks' false, point does not stop inside
    !> `intangible' text, and the `point_left' and `point_entered'
    !> functions run with the old and new point.  Own test.
    subroutine test_point_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed

        call define_tp_functions()
        call enter("*tp-point*", h, old)
        call insert("abcdef", st)
        call put_text_property(3_int64, 5_int64, sym("intangible"), t_value(), st)
        call goto_char(1_int64)
        call goto_char(4_int64)
        call check(error, buffers(cur)%pt == 4, "intangible ignored by default")
        if (allocated(error)) return
        inhibit_point_motion_hooks = .false.
        call goto_char(1_int64)
        call goto_char(4_int64)
        call check(error, buffers(cur)%pt == 5, "forward past intangible text")
        call goto_char(6_int64)
        call goto_char(4_int64)
        call check(error, buffers(cur)%pt == 3, "backward past intangible text")
        call remove_text_properties(1_int64, 7_int64, list(sym("intangible"), nil()), changed, st)
        call put_text_property(3_int64, 5_int64, sym("point_entered"), sym("tp_rec_entered"), st)
        call put_text_property(3_int64, 5_int64, sym("point_left"), sym("tp_rec_left"), st)
        tp_trace = ""
        call goto_char(1_int64)
        call goto_char(4_int64)
        call goto_char(6_int64)
        inhibit_point_motion_hooks = .true.
        ! Leaving the start of the text (3 -> 1) runs point_left; inside it,
        ! the intervals before and after point both have the functions, so
        ! each runs twice, as upstream.
        call check(error, tp_trace == " L3,1 E1,4 E1,4 L4,6 L4,6", "entered and left: '"//tp_trace//"'")
        call leave(h, old)
    end subroutine test_point_motion

    !> `text_property_list' lists the properties of a range;
    !> `extend_property_ranges' cuts and extends it;
    !> `copy_text_properties' copies them elsewhere; `copy_intervals_to_string'
    !> gives them to a string.  Own test.
    subroutine test_property_lists(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(text_property_range_type), allocatable :: ranges(:)
        type(value_type) :: s
        logical :: modified

        call enter("*tp-lists*", h, old)
        call insert("abcdef", st)
        call put_text_property(2_int64, 4_int64, sym("face"), sym("bold"), st)
        call put_text_property(5_int64, 6_int64, sym("mouse_face"), sym("highlight"), st)
        call text_property_list(cur, 1_int64, 7_int64, ranges, st)
        call check(error, size(ranges) == 2, "two ranges")
        if (allocated(error)) return
        call check(error, ranges(1)%start == 5 .and. ranges(1)%end == 6 .and. ranges(2)%start == 2 .and. &
                   ranges(2)%end == 4, "last first")
        if (allocated(error)) return
        call text_property_list(cur, 1_int64, 7_int64, ranges, st, sym("face"))
        call check(error, size(ranges) == 1, "only face")
        if (allocated(error)) return
        call extend_property_ranges(ranges, 4_int64, 6_int64)
        call check(error, ranges(1)%end == 6, "extended to the new end")
        if (allocated(error)) return
        s = make_string("bcd")
        call copy_intervals_to_string(s, buffers(cur)%text%intervals, 2_int64, 3_int64)
        call check(error, value_eq(string_get_text_property(1_int64, sym("face"), s), sym("bold")), "string properties")
        if (allocated(error)) return
        call check(error, compare_string_intervals(s, propertized_concat(propertize(make_string("bc"), &
                                                                                    list(sym("face"), sym("bold"))), &
                                                                         make_string("d")), 3_int64), "same properties")
        if (allocated(error)) return
        call copy_text_properties(2_int64, 4_int64, cur, 5_int64, cur, modified, st)
        call check(error, modified .and. value_eq(prop_at(6_int64, "face"), sym("bold")), "copied")
        call leave(h, old)
    end subroutine test_property_lists

    !> A `keymap' property at point puts its map first among the active
    !> keymaps, and a `local_map' property replaces the local map.
    !> Own test.
    subroutine test_local_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, km
        type(status_type) :: st
        type(value_type) :: map

        call enter("*tp-keymap*", h, old)
        call insert("abc", st)
        call make_sparse_keymap(km)
        call put_text_property(2_int64, 3_int64, sym("keymap"), make_keymap_ref(km), st)
        map = get_local_map(1_int64, "keymap")
        call check(error, nilp(map), "no keymap at 1")
        if (allocated(error)) return
        map = get_local_map(2_int64, "keymap")
        call check(error, value_eq(map, make_keymap_ref(km)), "the property's map at 2")
        if (allocated(error)) return
        ! At 3 the character after has none, but an insertion there would
        ! inherit it (rear-sticky).
        map = get_local_map(3_int64, "keymap")
        call check(error, value_eq(map, make_keymap_ref(km)), "inherited at 3")
        if (allocated(error)) return
        call put_text_property(1_int64, 2_int64, sym("local_map"), make_keymap_ref(km), st)
        map = get_local_map(1_int64, "local_map")
        call check(error, value_eq(map, make_keymap_ref(km)), "local_map property")
        call leave(h, old)
        call free_keymap(km, st)
    end subroutine test_local_map
end module test_textprop_fm
