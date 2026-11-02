! test_textprop_mod.f90 -- tests for intervals_mod and textprop_mod.
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

module test_textprop_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use value_mod, only: vtag, V_NULL, value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, FM_TEXT_READ_ONLY, FM_BUFFER_READ_ONLY, &
                         FM_ARGS_OUT_OF_RANGE, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use value_mod, only: list, value_type, make_keymap_ref
    use data_mod, only: value_eq, set
    use fns_mod, only: value_equal, put
    use buffer_mod, only: buffers, current, get_or_make_buffer, close_buffer, set_buffer, &
                          current_buffer, is_buffer_live, set_buffer_read_only, &
                          erase_buffer, inhibit_read_only, deactivate_mark_variable, UE_FIRST_CHANGE
    use undo_mod, only: undo_last_kind
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, delete_and_extract_region, point_max
    use textprop_mod, only: string_put_text_property, string_get_text_property, string_next_single_property_change, &
                            propertized_substring, propertized_concat, propertize
    use textprop_mod, only: add_text_properties, get_text_property, next_property_change, &
                            next_single_property_change, previous_property_change, &
                            previous_single_property_change, put_text_property, &
                            remove_list_of_text_properties, remove_text_properties, &
                            set_text_properties, text_properties_at, text_property_any, &
                            text_property_not_all, text_property_stickiness, text_property_range_type, &
                            text_property_list, extend_property_ranges, copy_text_properties, inhibit_point_motion_hooks
    use hooks_mod, only: property_function_type
    use keymap_mod, only: make_sparse_keymap, destroy_keymap, get_local_map
    use intervals_mod, only: copy_intervals_to_string, compare_string_intervals
    implicit none(type, external)
    private

    public :: collect_textprop_mod

    !> What the property functions recorded.
    character(len=:), allocatable :: tp_trace
    type(property_function_type), target :: tp_mod_fn, tp_behind_fn, tp_front_fn, tp_entered_fn, tp_left_fn

contains

    !> Collect all exported unit tests.
    subroutine collect_textprop_mod(testsuite)
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
    end subroutine collect_textprop_mod

    subroutine enter(name, h, old)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
    end subroutine enter

    subroutine leave(h, old)
        type(handle_type), intent(in) :: h, old
        type(status_type) :: st

        if (is_buffer_live(old)) call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine leave

    function prop_at(pos, name) result(v)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(pos, value_type(name=name), v, st)
    end function prop_at

    subroutine test_put_get(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: plist

        call enter("tt-put", h, old)
        call insert("abcdefgh", st)
        call put_text_property(3_int64, 6_int64, value_type(name="face"), value_type(name="bold"), st)
        call check(error, st%ok(), "put failed")
        if (allocated(error)) return
        call check(error, vtag(prop_at(2_int64, "face")) == V_NULL, "before range")
        if (allocated(error)) return
        call check(error, value_eq(prop_at(3_int64, "face"), value_type(name="bold")), "start of range")
        if (allocated(error)) return
        call check(error, value_eq(prop_at(5_int64, "face"), value_type(name="bold")), "end of range")
        if (allocated(error)) return
        call check(error, vtag(prop_at(6_int64, "face")) == V_NULL, "after range")
        if (allocated(error)) return
        ! A second property goes in front, as upstream.
        call put_text_property(4_int64, 5_int64, value_type(name="help-echo"), value_type(text="hi"), st)
        call text_properties_at(4_int64, plist, st)
        call check(error, value_equal(plist, list(value_type(name="help-echo"), value_type(text="hi"), value_type(name="face"), &
                                                  value_type(name="bold"))), &
                   "text-properties-at order")
        if (allocated(error)) return
        call text_properties_at(9_int64, plist, st)
        call check(error, st%ok() .and. .not. allocated(plist%a), "at point-max")
        call leave(h, old)
    end subroutine test_put_get

    subroutine test_add_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed

        call enter("tt-add", h, old)
        call insert("abcdef", st)
        call add_text_properties(1_int64, 4_int64, list(value_type(name="a"), value_type(1_int64)), changed, st)
        call check(error, changed, "first add did not change")
        if (allocated(error)) return
        call add_text_properties(2_int64, 3_int64, list(value_type(name="a"), value_type(1_int64)), changed, st)
        call check(error,.not. changed, "re-adding the same value changed")
        if (allocated(error)) return
        call add_text_properties(2_int64, 6_int64, list(value_type(name="a"), value_type(1_int64)), changed, st)
        call check(error, changed .and. value_eq(prop_at(5_int64, "a"), value_type(1_int64)), "extending")
        if (allocated(error)) return
        call add_text_properties(3_int64, 3_int64, list(value_type(name="b"), value_type(1_int64)), changed, st)
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
        call put_text_property(1_int64, 9_int64, value_type(name="face"), value_type(name="bold"), st)
        call put_text_property(1_int64, 9_int64, value_type(name="x"), value_type(1_int64), st)
        call remove_text_properties(3_int64, 5_int64, list(value_type(name="face"), value_type()), changed, st)
        call check(error, changed .and. vtag(prop_at(3_int64, "face")) == V_NULL .and. &
                   value_eq(prop_at(3_int64, "x"), value_type(1_int64)) .and. &
                   value_eq(prop_at(5_int64, "face"), value_type(name="bold")), "remove-text-properties")
        if (allocated(error)) return
        call remove_text_properties(3_int64, 5_int64, list(value_type(name="face"), value_type()), changed, st)
        call check(error,.not. changed, "removing again changed")
        if (allocated(error)) return
        call remove_list_of_text_properties(1_int64, 9_int64, list(value_type(name="x")), changed, st)
        call check(error, changed .and. vtag(prop_at(7_int64, "x")) == V_NULL, "remove-list-of-text-properties")
        if (allocated(error)) return
        call set_text_properties(2_int64, 4_int64, list(value_type(name="y"), value_type(2_int64)), changed, st)
        call text_properties_at(2_int64, plist, st)
        call check(error, value_equal(plist, list(value_type(name="y"), value_type(2_int64))), "set replaces all")
        if (allocated(error)) return
        call set_text_properties(1_int64, 9_int64, value_type(), changed, st)
        call check(error, changed .and. vtag(prop_at(2_int64, "y")) == V_NULL .and. vtag(prop_at(6_int64, "face")) == V_NULL, &
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
        call put_text_property(3_int64, 7_int64, value_type(name="face"), value_type(name="bold"), st)
        call put_text_property(5_int64, 7_int64, value_type(name="x"), value_type(1_int64), st)
        call next_property_change(1_int64, r, found, st)
        call check(error, found .and. r == 3, "next from 1")
        if (allocated(error)) return
        call next_property_change(3_int64, r, found, st)
        call check(error, found .and. r == 5, "next from 3")
        if (allocated(error)) return
        call next_single_property_change(3_int64, value_type(name="face"), r, found, st)
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
        call previous_single_property_change(7_int64, value_type(name="face"), r, found, st)
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
        call text_property_any(1_int64, 11_int64, value_type(name="face"), value_type(), r, found, st)
        call check(error, found .and. r == 1, "no intervals: nil everywhere")
        if (allocated(error)) return
        call put_text_property(4_int64, 6_int64, value_type(name="face"), value_type(name="bold"), st)
        call text_property_any(1_int64, 11_int64, value_type(name="face"), value_type(name="bold"), r, found, st)
        call check(error, found .and. r == 4, "any bold")
        if (allocated(error)) return
        call text_property_any(6_int64, 11_int64, value_type(name="face"), value_type(name="bold"), r, found, st)
        call check(error,.not. found, "no bold after 6")
        if (allocated(error)) return
        call text_property_not_all(4_int64, 11_int64, value_type(name="face"), value_type(name="bold"), r, found, st)
        call check(error, found .and. r == 6, "not-all")
        if (allocated(error)) return
        call text_property_not_all(4_int64, 6_int64, value_type(name="face"), value_type(name="bold"), r, found, st)
        call check(error,.not. found, "all bold in 4..6")
        call leave(h, old)
    end subroutine test_any_not_all

    subroutine test_category(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tt-cat", h, old)
        call insert("abc", st)
        call put(value_type(name="tt-my-category"), value_type(name="face"), value_type(name="italic"), st)
        call put_text_property(1_int64, 3_int64, value_type(name="category"), value_type(name="tt-my-category"), st)
        call check(error, value_eq(prop_at(1_int64, "face"), value_type(name="italic")), "face via category")
        if (allocated(error)) return
        call put_text_property(1_int64, 2_int64, value_type(name="face"), value_type(name="bold"), st)
        call check(error, value_eq(prop_at(1_int64, "face"), value_type(name="bold")), "own property wins")
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
        call put_text_property(3_int64, 5_int64, value_type(name="face"), value_type(name="bold"), st)   ! "cd"
        call goto_char(4_int64)
        call insert("XY", st)                                                     ! ab c XY d ef
        call check(error, value_eq(prop_at(3_int64, "face"), value_type(name="bold")) .and. &
                   vtag(prop_at(4_int64, "face")) == V_NULL .and. vtag(prop_at(5_int64, "face")) == V_NULL .and. &
                   value_eq(prop_at(6_int64, "face"), value_type(name="bold")) .and. vtag(prop_at(7_int64, "face")) == V_NULL, &
                   "after insertion inside")
        if (allocated(error)) return
        call goto_char(1_int64)
        call insert("Z", st)                                                      ! Z ab c XY d ef
        call check(error, value_eq(prop_at(4_int64, "face"), value_type(name="bold")), "shifted by insertion before")
        if (allocated(error)) return
        call delete_and_extract_region(1_int64, 5_int64, s, st)                  ! XY d ef
        call check(error, vtag(prop_at(1_int64, "face")) == V_NULL .and. value_eq(prop_at(3_int64, "face"), &
                                                                                  value_type(name="bold")) .and. &
                   vtag(prop_at(4_int64, "face")) == V_NULL, "after deletion")
        if (allocated(error)) return
        call delete_and_extract_region(1_int64, point_max(), s, st)
        call check(error, buffers(current)%text%intervals%n == 0, "all text gone: no intervals")
        call leave(h, old)
    end subroutine test_edits

    subroutine test_modifies(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: tick, chars_tick

        call enter("tt-mod", h, old)
        call insert("abc", st)
        tick = buffers(current)%text%modiff
        chars_tick = buffers(current)%text%chars_modiff
        call put_text_property(1_int64, 2_int64, value_type(name="face"), value_type(name="bold"), st)
        call check(error, buffers(current)%text%modiff > tick, "modiff did not advance")
        if (allocated(error)) return
        call check(error, buffers(current)%text%chars_modiff, chars_tick)
        if (allocated(error)) return
        ! As textprop.c:modify_text_properties: the first change of an
        ! unmodified buffer is recorded for undo, and the mark deactivated
        ! (prepare_to_modify_buffer_1).
        buffers(current)%text%save_modiff = buffers(current)%text%modiff
        deactivate_mark_variable = .false.
        call put_text_property(2_int64, 3_int64, value_type(name="face"), value_type(name="bold"), st)
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
        call add_text_properties(1_int64, 2_int64, list(value_type(name="a"), value_type(name="b"), value_type(name="c")), &
                                 changed, st)
        call check(error, st%code == FM_GENERIC_ERROR .and. .not. changed, "odd plist")
        if (allocated(error)) return
        call put_text_property(0_int64, 2_int64, value_type(name="a"), value_type(name="b"), st)
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
        call put_text_property(3_int64, 5_int64, value_type(name="read_only"), value_type(.true.), st)
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
        call put_text_property(4_int64, 6_int64, value_type(name="rear_nonsticky"), value_type(.true.), st)
        call check(error, st%code, FM_TEXT_READ_ONLY)   ! changing read-only text
        if (allocated(error)) return
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call put_text_property(4_int64, 6_int64, value_type(name="rear_nonsticky"), value_type(.true.), st)
        call set(value_type(name="inhibit_read_only"), value_type(), st)
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
            call set(value_type(name="inhibit_read_only"), list(value_type(name="abcdefg")), st)
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
            call set(value_type(name="inhibit_read_only"), value_type(), st)
            ! Two inhibit-read-only sections separated by immutable text.
            call delete_and_extract_region(17_int64, 27_int64, s, st)
            call check(error, st%code, FM_BUFFER_READ_ONLY)
            if (allocated(error)) exit run
            call set(value_type(name="inhibit_read_only"), list(value_type(name="abcdefg")), st)
            ! From an inhibit-read-only section into protected text.
            call delete_and_extract_region(26_int64, 37_int64, s, st)
            call check(error, st%code, FM_TEXT_READ_ONLY)
        end block run

        call set(value_type(name="inhibit_read_only"), value_type(), st)
        call leave(h, old)

    contains

        subroutine fill(st)
            type(status_type), intent(out) :: st

            call insert("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ", st)
            call put_text_property(1_int64, 6_int64, value_type(name="inhibit_read_only"), value_type(.true.), st)
            call put_text_property(6_int64, 11_int64, value_type(name="read_only"), value_type(name="abcdefg"), st)
            call put_text_property(11_int64, 16_int64, value_type(name="inhibit_read_only"), value_type(.true.), st)
            call put_text_property(16_int64, 21_int64, value_type(name="inhibit_read_only"), value_type(name="yes"), st)
            call put_text_property(26_int64, 31_int64, value_type(name="inhibit_read_only"), value_type(.true.), st)
            call put_text_property(31_int64, 37_int64, value_type(name="read_only"), value_type(name="not-suppressed"), st)
        end subroutine fill

        !> (let ((inhibit-read-only t)) (erase-buffer) (insert template))
        subroutine refill()
            type(value_type) :: saved
            type(status_type) :: st2

            saved = inhibit_read_only()
            call set(value_type(name="inhibit_read_only"), value_type(.true.), st2)
            call erase_buffer(st2)
            call fill(st2)
            call set(value_type(name="inhibit_read_only"), saved, st2)
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
        call put_text_property(1_int64, 2_int64, value_type(name="cursor"), value_type(.true.), st)
        call put_text_property(1_int64, 2_int64, value_type(name="syntax_table"), value_type(.true.), st)
        call put_text_property(1_int64, 2_int64, value_type(name="fill_space"), value_type(.true.), st)
        call check(error, text_property_stickiness(value_type(name="cursor"), 2_int64) == -1, "cursor is rear-sticky")
        if (allocated(error)) return
        call check(error, text_property_stickiness(value_type(name="syntax_table"), 2_int64) == 0, "syntax_table")
        if (allocated(error)) return
        call check(error, text_property_stickiness(value_type(name="fill_space"), 2_int64) == 0, "fill_space")
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

        s = value_type(text="abcdef")
        call string_put_text_property(1_int64, 3_int64, value_type(name="face"), value_type(name="bold"), s, st)
        call check(error, st%ok(), "put")
        if (allocated(error)) return
        call check(error, vtag(string_get_text_property(0_int64, value_type(name="face"), s)) == V_NULL .and. &
                   value_eq(string_get_text_property(1_int64, value_type(name="face"), s), value_type(name="bold")) .and. &
                   value_eq(string_get_text_property(2_int64, value_type(name="face"), s), value_type(name="bold")) .and. &
                   vtag(string_get_text_property(3_int64, value_type(name="face"), s)) == V_NULL, "face on bc")
        if (allocated(error)) return
        call check(error, value_equal(s, value_type(text="abcdef")), "equal ignores properties")
        if (allocated(error)) return
        call string_next_single_property_change(0_int64, value_type(name="face"), s, pos, found)
        call check(error, found .and. pos == 1, "next change at 1")
        if (allocated(error)) return
        call string_next_single_property_change(1_int64, value_type(name="face"), s, pos, found)
        call check(error, found .and. pos == 3, "next change at 3")
        if (allocated(error)) return
        call string_next_single_property_change(3_int64, value_type(name="face"), s, pos, found)
        call check(error,.not. found, "no change after 3")
        if (allocated(error)) return
        call string_put_text_property(0_int64, 7_int64, value_type(name="face"), value_type(name="bold"), s, st)
        call check(error,.not. st%ok(), "args-out-of-range")
        if (allocated(error)) return
        part = propertized_substring(s, 2_int64, 5_int64)
        call check(error, value_equal(part, value_type(text="cde")) .and. &
                   value_eq(string_get_text_property(0_int64, value_type(name="face"), part), value_type(name="bold")) .and. &
                   vtag(string_get_text_property(1_int64, value_type(name="face"), part)) == V_NULL, "substring keeps c's face")
        if (allocated(error)) return
        both = propertized_concat(part, propertize(value_type(text="xy"), list(value_type(name="cursor"), value_type(1_int64))))
        call check(error, value_equal(both, value_type(text="cdexy")) .and. &
                   value_eq(string_get_text_property(0_int64, value_type(name="face"), both), value_type(name="bold")) .and. &
                   vtag(string_get_text_property(3_int64, value_type(name="face"), both)) == V_NULL .and. &
                   value_eq(string_get_text_property(4_int64, value_type(name="cursor"), both), value_type(1_int64)), &
                   "concat keeps both")
    end subroutine test_string_properties

    ! ------------------------------------------------------------------
    ! Modification hooks, point motion, property lists, keymaps
    ! ------------------------------------------------------------------

    ! Property functions recording their calls in `tp_trace'.
    subroutine tp_rec_mod(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" M"//num(beginning)//","//num(end)
    end subroutine tp_rec_mod

    ! See `tp_rec_mod'.
    subroutine tp_rec_behind(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" B"//num(beginning)//","//num(end)
    end subroutine tp_rec_behind

    ! See `tp_rec_mod'.
    subroutine tp_rec_front(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        tp_trace = tp_trace//" F"//num(beginning)//","//num(end)
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
        character(len=24) :: buffer

        write (buffer, "(i0)") n
        s = trim(buffer)
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
        call put_text_property(2_int64, 4_int64, value_type(name="modification_hooks"), list(value_type(name="tp_rec_mod")), st)
        call put_text_property(1_int64, 2_int64, value_type(name="insert_behind_hooks"), list(value_type(name="tp_rec_behind")), st)
        call put_text_property(4_int64, 5_int64, value_type(name="insert_in_front_hooks"), list(value_type(name="tp_rec_front")), &
                               st)
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
        call put_text_property(3_int64, 5_int64, value_type(name="intangible"), value_type(.true.), st)
        call goto_char(1_int64)
        call goto_char(4_int64)
        call check(error, buffers(current)%pt == 4, "intangible ignored by default")
        if (allocated(error)) return
        inhibit_point_motion_hooks = .false.
        call goto_char(1_int64)
        call goto_char(4_int64)
        call check(error, buffers(current)%pt == 5, "forward past intangible text")
        call goto_char(6_int64)
        call goto_char(4_int64)
        call check(error, buffers(current)%pt == 3, "backward past intangible text")
        call remove_text_properties(1_int64, 7_int64, list(value_type(name="intangible"), value_type()), changed, st)
        call put_text_property(3_int64, 5_int64, value_type(name="point_entered"), value_type(name="tp_rec_entered"), st)
        call put_text_property(3_int64, 5_int64, value_type(name="point_left"), value_type(name="tp_rec_left"), st)
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
        call put_text_property(2_int64, 4_int64, value_type(name="face"), value_type(name="bold"), st)
        call put_text_property(5_int64, 6_int64, value_type(name="mouse_face"), value_type(name="highlight"), st)
        call text_property_list(current, 1_int64, 7_int64, ranges, st)
        call check(error, size(ranges) == 2, "two ranges")
        if (allocated(error)) return
        call check(error, ranges(1)%start == 5 .and. ranges(1)%end == 6 .and. ranges(2)%start == 2 .and. &
                   ranges(2)%end == 4, "last first")
        if (allocated(error)) return
        call text_property_list(current, 1_int64, 7_int64, ranges, st, value_type(name="face"))
        call check(error, size(ranges) == 1, "only face")
        if (allocated(error)) return
        call extend_property_ranges(ranges, 4_int64, 6_int64)
        call check(error, ranges(1)%end == 6, "extended to the new end")
        if (allocated(error)) return
        s = value_type(text="bcd")
        call copy_intervals_to_string(s, buffers(current)%text%intervals, 2_int64, 3_int64)
        call check(error, value_eq(string_get_text_property(1_int64, value_type(name="face"), s), value_type(name="bold")), &
                   "string properties")
        if (allocated(error)) return
        call check(error, compare_string_intervals(s, propertized_concat(propertize(value_type(text="bc"), &
                                                                                    list(value_type(name="face"), &
                                                                                         value_type(name="bold"))), &
                                                                         value_type(text="d")), 3_int64), "same properties")
        if (allocated(error)) return
        call copy_text_properties(2_int64, 4_int64, current, 5_int64, current, modified, st)
        call check(error, modified .and. value_eq(prop_at(6_int64, "face"), value_type(name="bold")), "copied")
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
        call put_text_property(2_int64, 3_int64, value_type(name="keymap"), make_keymap_ref(km), st)
        map = get_local_map(1_int64, "keymap")
        call check(error,.not. allocated(map%a), "no keymap at 1")
        if (allocated(error)) return
        map = get_local_map(2_int64, "keymap")
        call check(error, value_eq(map, make_keymap_ref(km)), "the property's map at 2")
        if (allocated(error)) return
        ! At 3 the character after has none, but an insertion there would
        ! inherit it (rear-sticky).
        map = get_local_map(3_int64, "keymap")
        call check(error, value_eq(map, make_keymap_ref(km)), "inherited at 3")
        if (allocated(error)) return
        call put_text_property(1_int64, 2_int64, value_type(name="local_map"), make_keymap_ref(km), st)
        map = get_local_map(1_int64, "local_map")
        call check(error, value_eq(map, make_keymap_ref(km)), "local_map property")
        call leave(h, old)
        call destroy_keymap(km, st)
    end subroutine test_local_map
end module test_textprop_mod
