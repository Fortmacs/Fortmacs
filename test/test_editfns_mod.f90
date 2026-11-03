! test_editfns_mod.f90 -- tests for editfns_mod.
! Ports GNU Emacs 31.1 test/src/editfns-tests.el in part; the rest need
! `format', `replace-region-contents' or undo marker adjustments (the
! change-hook cases are in test_insdel_mod).
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

!> Ports find-fields, test-group-name, the transpose-regions tests,
!> editfns-tests--bug76124 and test-translate-region-internal; the other
!> tests are our own.
module test_editfns_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, FM_ARGS_OUT_OF_RANGE, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, format_error, &
                         format_message
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, close_buffer, set_buffer, &
                          current_buffer, is_buffer_live, erase_buffer
    use marker_mod, only: make_marker, set_marker, destroy_marker, marker_position
    use insdel_mod, only: insert
    use editfns_mod, only: is_bob, is_bol, buffer_size, buffer_size_of, buffer_string, buffer_substring, &
                           byte_to_position, char_after, char_before, delete_and_extract_region, &
                           delete_region, is_eob, is_eol, following_char, gap_position, goto_char, &
                           goto_char_marker, insert_char_n, narrow_to_region, point, point_marker, &
                           point_max, point_max_marker, point_min, point_min_marker, position_bytes, &
                           preceding_char, widen, field_beginning, field_end, field_string, &
                           constrain_to_field, line_beginning_position, line_end_position, &
                           delete_field, subst_char_in_region, interactive_region, check_mark, mark_marker
    use editfns_mod, only: insert, insert_before_markers, insert_and_inherit, insert_before_markers_and_inherit, &
                           buffer_substring_no_properties, compare_buffer_substrings, char_to_string, string_to_char, &
                           is_char_equal, ngettext, emacs_pid, group_name, group_real_gid, field_string_no_properties
    use fileio_mod, only: group_gid
    use sysdep_mod, only: emacs_getpid, emacs_getgid
    use search_mod, only: case_fold_search
    use xdisp_mod, only: message, current_message
    use callproc_mod, only: execute_process
    use value_mod, only: xfixnum, xstring, vtag, V_INTEGER, V_NULL
    use textprop_mod, only: string_get_text_property
    use buffer_mod, only: buffers, current, transient_mark_mode
    use callint_mod, only: mark_even_if_inactive
    use error_mod, only: FM_MARK_INACTIVE
    use value_mod, only: list, value_type
    use textprop_mod, only: add_text_properties, put_text_property, get_text_property
    use value_mod, only: value_type
    use data_mod, only: value_eq
    use editfns_mod, only: transpose_regions, translate_region_internal, save_excursion_begin, save_excursion_end, &
                           save_restriction_begin, save_restriction_end, excursion_type, restriction_type, &
                           internal_labeled_narrow_to_region, internal_labeled_widen, labeled_narrow_to_region_begin, &
                           labeled_narrow_to_region_end, labeled_restrictions_remove_in_current_buffer, &
                           reset_outermost_restrictions_begin, reset_outermost_restrictions_end, pos_bol, pos_eol
    use marker_mod, only: copy_marker_at
    use value_mod, only: symbols, xsymbol, V_NAME
    use undo_mod, only: undo_boundary, undo_last_kind
    use buffer_mod, only: UE_BOUNDARY
    use simple_mod, only: undo
    use callint_mod, only: prefix_arg_type
    use keyboard_mod, only: last_command
    use insdel_mod, only: before_change_functions
    use subr_mod, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use cmds_mod, only: end_of_line, delete_char
    use chartab_mod, only: make_char_table, char_table_set, destroy_char_table
    use character_mod, only: MAX_CHAR
    implicit none(type, external)
    private

    public :: collect_editfns_mod

    character(len=*), parameter :: euro = char(226)//char(130)//char(172)

contains

    !> Collect all exported unit tests.
    subroutine collect_editfns_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("point-and-bounds", test_bounds), &
                    new_unittest("goto-char-clips", test_goto), &
                    new_unittest("goto-char-marker", test_goto_marker), &
                    new_unittest("position-bytes", test_position_bytes), &
                    new_unittest("chars-around-point", test_chars_around), &
                    new_unittest("line-predicates", test_lines), &
                    new_unittest("narrow-and-widen", test_narrow), &
                    new_unittest("narrowing-limits-editing", test_narrow_edit), &
                    new_unittest("buffer-substring", test_substring), &
                    new_unittest("delete-region", test_delete), &
                    new_unittest("insert-char-count", test_insert_char), &
                    new_unittest("point-markers", test_point_markers), &
                    new_unittest("erase-buffer-widens", test_erase), &
                    new_unittest("find-fields", test_find_fields), &
                    new_unittest("prompt-field", test_prompt_field), &
                    new_unittest("subst-char-in-region", test_subst_char), &
                    new_unittest("interactive-region", test_interactive_region), &
                    new_unittest("insert-strings", test_insert_strings), &
                    new_unittest("insert-strings-markers-inherit", test_insert_markers_inherit), &
                    new_unittest("buffer-substring-properties", test_substring_properties), &
                    new_unittest("field-string", test_field_string), &
                    new_unittest("compare-buffer-substrings", test_compare_substrings), &
                    new_unittest("char-and-string", test_char_string), &
                    new_unittest("char-equal", test_char_equal), &
                    new_unittest("ngettext-and-ids", test_ngettext_ids), &
                    new_unittest("current-message", test_current_message), &
                    new_unittest("test-group-name", test_group_name), &
                    new_unittest("transpose-ascii-regions-test", test_transpose_ascii), &
                    new_unittest("transpose-nonascii-regions-test-1", test_transpose_nonascii_1), &
                    new_unittest("transpose-nonascii-regions-test-2", test_transpose_nonascii_2), &
                    new_unittest("editfns-tests--transpose-equal-but-not", test_transpose_equal_but_not), &
                    new_unittest("editfns-tests--transpose-regions", test_transpose_regions), &
                    new_unittest("transpose-regions-errors", test_transpose_errors), &
                    new_unittest("editfns-tests--bug76124", test_bug76124), &
                    new_unittest("test-translate-region-internal", test_translate_region_internal), &
                    new_unittest("translate-region-string", test_translate_string), &
                    new_unittest("save-excursion", test_save_excursion), &
                    new_unittest("save-restriction", test_save_restriction), &
                    new_unittest("labeled-restrictions", test_labeled_restrictions) &
                    ]
    end subroutine collect_editfns_mod

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

    subroutine test_bounds(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: size

        call enter("te-bounds", h, old)
        call check(error, point() == 1 .and. point_min() == 1 .and. point_max() == 1, "empty")
        if (allocated(error)) return
        call insert("ab"//euro, st)
        call check(error, point() == 4 .and. point_max() == 4 .and. buffer_size() == 3, "after insert")
        if (allocated(error)) return
        call buffer_size_of(h, size, st)
        call check(error, size, 3_int64)
        if (allocated(error)) return
        call check(error, gap_position(), 4_int64)
        call leave(h, old)
    end subroutine test_bounds

    subroutine test_goto(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-goto", h, old)
        call insert("hello", st)
        call goto_char(3_int64)
        call check(error, point(), 3_int64)
        if (allocated(error)) return
        call goto_char(-10_int64)
        call check(error, point(), 1_int64)
        if (allocated(error)) return
        call goto_char(99_int64)
        call check(error, point(), 6_int64)
        call leave(h, old)
    end subroutine test_goto

    !> As marker-goto-char-from-other-buffer: the byte position must be
    !> recomputed when the marker belongs to another buffer.
    subroutine test_goto_marker(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: mb, ascii, old, old2, m
        type(status_type) :: st
        integer(int64) :: bytepos
        logical :: found

        call enter("te-gm-mb", mb, old)
        call insert(euro//euro//euro, st)
        call make_marker(m)
        call set_marker(m, 3_int64, st)              ! char 3 = byte 7 here
        call enter("te-gm-ascii", ascii, old2)
        call insert("abcdef", st)
        call goto_char_marker(m, st)
        call check(error, point(), 3_int64)
        if (allocated(error)) return
        call position_bytes(point(), bytepos, found)
        call check(error, found .and. bytepos == 3, "point byte position")
        call destroy_marker(m, st)
        call leave(ascii, old2)
        call leave(mb, old)
    end subroutine test_goto_marker

    subroutine test_position_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: p
        logical :: found

        call enter("te-pb", h, old)
        call insert("a"//euro//"b", st)            ! bytes: a=1, euro=2..4, b=5
        call position_bytes(3_int64, p, found)
        call check(error, found .and. p == 5, "position-bytes 3")
        if (allocated(error)) return
        call position_bytes(10_int64, p, found)
        call check(error,.not. found, "out of range accepted")
        if (allocated(error)) return
        call byte_to_position(3_int64, p, found)    ! inside the euro sign
        call check(error, found .and. p == 2, "byte-to-position mid-char")
        if (allocated(error)) return
        call byte_to_position(6_int64, p, found)    ! Z_BYTE
        call check(error, found .and. p == 4, "byte-to-position at Z")
        call leave(h, old)
    end subroutine test_position_bytes

    subroutine test_chars_around(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int32) :: c
        logical :: found

        call enter("te-chars", h, old)
        call insert("a"//euro, st)
        call check(error, following_char(), 0_int32)
        if (allocated(error)) return
        call check(error, preceding_char(), int(z'20AC', int32))
        if (allocated(error)) return
        call goto_char(1_int64)
        call check(error, following_char(), int(iachar("a"), int32))
        if (allocated(error)) return
        call check(error, preceding_char(), 0_int32)
        if (allocated(error)) return
        call char_after(c, found, 2_int64)
        call check(error, found .and. c == int(z'20AC', int32), "char-after 2")
        if (allocated(error)) return
        call char_after(c, found, 3_int64)
        call check(error,.not. found, "char-after at end")
        if (allocated(error)) return
        call char_before(c, found, 3_int64)
        call check(error, found .and. c == int(z'20AC', int32), "char-before 3")
        if (allocated(error)) return
        call char_before(c, found)
        call check(error,.not. found, "char-before at bob")
        call leave(h, old)
    end subroutine test_chars_around

    subroutine test_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-lines", h, old)
        call insert("ab"//new_line("a")//"cd", st)
        call check(error, is_eol() .and. is_eob() .and. .not. is_bol(), "at end")
        if (allocated(error)) return
        call goto_char(4_int64)
        call check(error, is_bol() .and. .not. is_eol(), "start of line 2")
        if (allocated(error)) return
        call goto_char(3_int64)
        call check(error, is_eol() .and. .not. is_bol(), "end of line 1")
        if (allocated(error)) return
        call goto_char(1_int64)
        call check(error, is_bob() .and. is_bol(), "beginning")
        call leave(h, old)
    end subroutine test_lines

    subroutine test_narrow(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-narrow", h, old)
        call insert("0123456789", st)                 ! point at 11
        call narrow_to_region(7_int64, 3_int64, st)   ! reversed is fine
        call check(error, point_min() == 3 .and. point_max() == 7, "bounds")
        if (allocated(error)) return
        call check(error, point(), 7_int64)           ! clamped into the region
        if (allocated(error)) return
        call check(error, buffer_string(), "2345")
        if (allocated(error)) return
        call check(error, is_bob() .eqv. .false., "is_bob")
        if (allocated(error)) return
        call narrow_to_region(0_int64, 5_int64, st)
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        if (allocated(error)) return
        call widen()
        call check(error, point_min() == 1 .and. point_max() == 11, "widened")
        if (allocated(error)) return
        call check(error, buffer_string(), "0123456789")
        call leave(h, old)
    end subroutine test_narrow

    subroutine test_narrow_edit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-narrow-edit", h, old)
        call insert("abcdef", st)
        call narrow_to_region(2_int64, 5_int64, st)   ! "bcd"
        call goto_char(5_int64)
        call insert("X", st)                          ! extends the region
        call check(error, point_max(), 6_int64)
        if (allocated(error)) return
        call check(error, buffer_string(), "bcdX")
        if (allocated(error)) return
        call delete_region(1_int64, 3_int64, st)       ! starts before point-min
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        if (allocated(error)) return
        call delete_region(2_int64, 4_int64, st)
        call check(error, buffer_string(), "dX")
        if (allocated(error)) return
        call widen()
        call check(error, buffer_string(), "adXef")
        call leave(h, old)
    end subroutine test_narrow_edit

    subroutine test_substring(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: s

        call enter("te-substr", h, old)
        call insert("x"//euro//"yz", st)
        call buffer_substring(4_int64, 2_int64, s, st)
        call check(error, s, euro//"y")
        if (allocated(error)) return
        call buffer_substring(1_int64, 9_int64, s, st)
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        if (allocated(error)) return
        call check(error, st%detail, "1, 9")
        call leave(h, old)
    end subroutine test_substring

    !> The characters are replaced in place: point and text properties
    !> stay; the region is validated, and the characters must have the
    !> same byte length (editfns.c:Fsubst_char_in_region).
    subroutine test_subst_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: v

        call enter("te-subst", h, old)
        call insert("a"//char(9)//"b"//char(9)//"c", st)
        call put_text_property(2_int64, 3_int64, value_type(name="face"), value_type(name="bold"), st)
        call goto_char(3_int64)
        call subst_char_in_region(6_int64, 1_int64, 9, 32, st)
        call check(error, st%ok() .and. buffer_string() == "a b c" .and. point() == 3, "subst: "//buffer_string())
        if (allocated(error)) return
        call get_text_property(2_int64, value_type(name="face"), v, st)
        call check(error, value_eq(v, value_type(name="bold")), "property kept")
        if (allocated(error)) return
        call subst_char_in_region(1_int64, 9_int64, 32, 95, st)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE .and. format_error(st) == "Args out of range: 1, 9", &
                   format_error(st))
        if (allocated(error)) return
        call subst_char_in_region(1_int64, 6_int64, 97, 233, st)
        call check(error, st%code == FM_GENERIC_ERROR .and. format_error(st) == &
                   format_message("Characters in `subst_char_in_region' have different byte-lengths"), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string(), "a b c")
        call leave(h, old)
    end subroutine test_subst_char

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: s

        call enter("te-delete", h, old)
        call insert("hello world", st)
        call delete_and_extract_region(6_int64, 12_int64, s, st)
        call check(error, s, " world")
        if (allocated(error)) return
        call delete_and_extract_region(3_int64, 3_int64, s, st)
        call check(error, s == "" .and. st%ok(), "empty extraction")
        if (allocated(error)) return
        call delete_region(1_int64, 2_int64, st)
        call check(error, buffer_string(), "ello")
        call leave(h, old)
    end subroutine test_delete

    subroutine test_insert_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-insert-char", h, old)
        call insert_char_n(int(z'20AC', int32), 2000_int64, st)   ! 6000 bytes: two chunks
        call check(error, st%ok(), "insert-char failed")
        if (allocated(error)) return
        call check(error, point_max(), 2001_int64)
        if (allocated(error)) return
        call check(error, len(buffer_string()), 6000)
        if (allocated(error)) return
        call insert_char_n(int(iachar("a"), int32), 0_int64, st)
        call check(error, point_max(), 2001_int64)
        if (allocated(error)) return
        call insert_char_n(-1_int32, 1_int64, st)
        call check(error,.not. st%ok(), "invalid character accepted")
        call leave(h, old)
    end subroutine test_insert_char

    subroutine test_point_markers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, pm, lo, hi
        type(status_type) :: st
        integer(int64) :: p

        call enter("te-pm", h, old)
        call insert("abcdef", st)
        call narrow_to_region(2_int64, 5_int64, st)
        call goto_char(3_int64)
        call point_marker(pm)
        call point_min_marker(lo)
        call point_max_marker(hi)
        call marker_position(pm, p, st)
        call check(error, p, 3_int64)
        if (allocated(error)) return
        call marker_position(lo, p, st)
        call check(error, p, 2_int64)
        if (allocated(error)) return
        call marker_position(hi, p, st)
        call check(error, p, 5_int64)
        call destroy_marker(pm, st)
        call destroy_marker(lo, st)
        call destroy_marker(hi, st)
        call leave(h, old)
    end subroutine test_point_markers

    subroutine test_erase(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-erase", h, old)
        call insert("abcdef", st)
        call narrow_to_region(2_int64, 4_int64, st)
        call erase_buffer(st)
        call check(error, buffer_size() == 0 .and. point_min() == 1 .and. point_max() == 1, &
                   "erase-buffer must widen and delete everything")
        call leave(h, old)
    end subroutine test_erase

    ! editfns-tests.el: find-fields
    subroutine test_find_fields(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed

        call enter("te-fields", h, old)
        call insert("foobarzot", st)
        call add_text_properties(4_int64, 7_int64, list(value_type(name="field"), value_type(name="bar")), changed, st)
        call goto_char(point_min())
        call check(error, field_beginning(point(), .false.) == point_min() .and. &
                   field_end(point(), .false.) == 4)
        if (allocated(error)) return
        call goto_char(5_int64)
        call check(error, field_beginning(point(), .false.) == 4 .and. field_end(point(), .false.) == 7)
        if (allocated(error)) return
        call goto_char(8_int64)
        call check(error, field_beginning(point(), .false.) == 7 .and. &
                   field_end(point(), .false.) == point_max())
        if (allocated(error)) return
        call check(error, field_string_no_properties(5_int64) == "bar")
        if (allocated(error)) return
        call delete_field(5_int64, st)
        call check(error, buffer_string() == "foozot")
        call leave(h, old)
    end subroutine test_find_fields

    !> A minibuffer-style prompt (a read-only, front-sticky,
    !> rear-nonsticky field): line motion stops after it.
    subroutine test_prompt_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        logical :: changed

        call enter("te-prompt", h, old)
        call insert("Find file: ", st)
        call add_text_properties(1_int64, point(), &
                                 list(value_type(name="field"), value_type(.true.), value_type(name="front_sticky"), &
                                      value_type(.true.), &
                                      value_type(name="rear_nonsticky"), value_type(.true.)), changed, st)
        call insert("/tmp/x", st)
        call check(error, line_beginning_position() == 12, "C-a stops after the prompt")
        if (allocated(error)) return
        call check(error, line_end_position() == 18)
        if (allocated(error)) return
        call check(error, constrain_to_field(3_int64, 15_int64, .false., .false.) == 12)
        if (allocated(error)) return
        ! At the prompt end (the input field's start), escaping stays put.
        call goto_char(12_int64)
        call check(error, line_beginning_position() == 12)
        if (allocated(error)) return
        ! In the prompt itself, the line starts at the buffer start.
        call goto_char(5_int64)
        call check(error, line_beginning_position() == 1)
        call leave(h, old)
    end subroutine test_prompt_field

    !> The `r' code of call-interactively (callint.c:check_mark): point
    !> and the mark as they are, even outside the accessible part; errors
    !> for no mark and an inactive one.
    subroutine test_interactive_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, m
        type(status_type) :: st
        integer(int64) :: b, e
        logical :: saved

        call enter("te-ireg", h, old)
        call insert("abcdefghij", st)
        call interactive_region(b, e, st)
        call check(error, format_error(st), "The mark is not set now, so there is no region")
        if (allocated(error)) return
        call check_mark(.false., st)
        call check(error, format_error(st), "The mark is not set now")
        if (allocated(error)) return
        call mark_marker(m)
        call set_marker(m, 2_int64, st)
        buffers(current)%mark_active = .true.
        call narrow_to_region(4_int64, 8_int64, st)
        call goto_char(6_int64)
        call interactive_region(b, e, st)
        call check(error, st%ok() .and. b == 2 .and. e == 6, "the mark outside the narrowing")
        if (allocated(error)) return
        saved = mark_even_if_inactive
        buffers(current)%mark_active = .false.
        mark_even_if_inactive = .false.
        call interactive_region(b, e, st)
        mark_even_if_inactive = saved
        call check(error, st%code == FM_MARK_INACTIVE .and. transient_mark_mode%value, "inactive mark")
        call leave(h, old)
    end subroutine test_interactive_region

    !> editfns.c:Finsert with several strings: each is inserted in turn
    !> at point, which ends up after them.
    subroutine test_insert_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: s

        call enter("te-insert-strings", h, old)
        call insert(st, "Hello, ", "w"//char(195)//char(182)//"rld", "!")
        call check(error, st%ok() .and. buffer_string() == "Hello, w"//char(195)//char(182)//"rld!", buffer_string())
        if (allocated(error)) return
        call check(error, point(), 14_int64)
        if (allocated(error)) return
        call goto_char(1_int64)
        call insert(st, "a", "b", "c", "d", "e", "f", "g", "h")
        call check(error, st%ok() .and. point() == 9, "eight strings")
        if (allocated(error)) return
        call buffer_substring_no_properties(1_int64, 9_int64, s, st)
        call check(error, s, "abcdefgh")
        call leave(h, old)
    end subroutine test_insert_strings

    !> insert-before-markers moves the markers at point after the text,
    !> insert does not; the -and-inherit forms take the sticky text
    !> properties of the text before point.
    subroutine test_insert_markers_inherit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, m
        type(status_type) :: st
        type(value_type) :: v
        integer(int64) :: p

        call enter("te-insert-markers", h, old)
        call insert("ab", st)
        call make_marker(m)
        call set_marker(m, 3_int64, st)
        call insert(st, "c", "d")
        call marker_position(m, p, st)
        call check(error, p, 3_int64)
        if (allocated(error)) return
        call goto_char(3_int64)
        call insert_before_markers(st, "x", "y")
        call marker_position(m, p, st)
        call check(error, st%ok() .and. p == 5 .and. buffer_string() == "abxycd", buffer_string())
        if (allocated(error)) return
        call put_text_property(1_int64, 2_int64, value_type(name="face"), value_type(name="bold"), st)
        call goto_char(2_int64)
        call insert_and_inherit(st, "1", "2")
        call get_text_property(3_int64, value_type(name="face"), v, st)
        call check(error, value_eq(v, value_type(name="bold")), "inherited")
        if (allocated(error)) return
        call insert(st, "3", "4")
        call get_text_property(5_int64, value_type(name="face"), v, st)
        call check(error,.not. allocated(v%a), "not inherited")
        if (allocated(error)) return
        call goto_char(2_int64)
        call set_marker(m, 2_int64, st)
        call insert_before_markers_and_inherit(st, "5", "6")
        call get_text_property(3_int64, value_type(name="face"), v, st)
        call marker_position(m, p, st)
        call check(error, value_eq(v, value_type(name="bold")) .and. p == 4, "both")
        call destroy_marker(m, st)
        call leave(h, old)
    end subroutine test_insert_markers_inherit

    !> buffer-substring copies the text properties into a string value;
    !> buffer-substring-no-properties gives the text alone.
    subroutine test_substring_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: v
        character(len=:), allocatable :: s

        call enter("te-substr-props", h, old)
        call insert("abcdef", st)
        call put_text_property(3_int64, 5_int64, value_type(name="face"), value_type(name="bold"), st)
        call buffer_substring(6_int64, 2_int64, v, st)
        call check(error, st%ok() .and. xstring(v) == "bcde", "text")
        if (allocated(error)) return
        call check(error, vtag(string_get_text_property(0_int64, value_type(name="face"), v)) == V_NULL .and. &
                   value_eq(string_get_text_property(1_int64, value_type(name="face"), v), value_type(name="bold")) .and. &
                   value_eq(string_get_text_property(2_int64, value_type(name="face"), v), value_type(name="bold")) .and. &
                   vtag(string_get_text_property(3_int64, value_type(name="face"), v)) == V_NULL, "face on cd")
        if (allocated(error)) return
        call buffer_substring_no_properties(2_int64, 6_int64, s, st)
        call check(error, s, "bcde")
        if (allocated(error)) return
        call buffer_substring(0_int64, 3_int64, v, st)
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        call leave(h, old)
    end subroutine test_substring_properties

    !> field-string keeps the properties of the field's text;
    !> field-string-no-properties drops them.
    subroutine test_field_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: v

        call enter("te-field-string", h, old)
        call insert("ab", st)
        call put_text_property(1_int64, 3_int64, value_type(name="field"), value_type(name="one"), st)
        call insert("cd", st)
        v = field_string(1_int64)
        call check(error, xstring(v) == "ab" .and. &
                   value_eq(string_get_text_property(1_int64, value_type(name="field"), v), value_type(name="one")), &
                   "with properties")
        if (allocated(error)) return
        call check(error, field_string_no_properties(4_int64), "cd")
        call leave(h, old)
    end subroutine test_field_string

    !> compare-buffer-substrings: the sign and the count of matching
    !> characters, the defaults, either order, and case folding with
    !> case-fold-search.
    subroutine test_compare_substrings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, h2, old2
        type(status_type) :: st
        integer(int64) :: r
        logical :: saved

        call enter("te-compare-2", h2, old2)
        call insert("abD", st)
        call enter("te-compare", h, old)
        call insert("abc", st)
        saved = case_fold_search%value
        case_fold_search = .false.
        call compare_buffer_substrings(r, st, buffer2=h2)
        ! "c" (99) is greater than "D" (68).
        call check(error, st%ok() .and. r == 3, "abc vs abD")
        if (allocated(error)) return
        case_fold_search = .true.
        call compare_buffer_substrings(r, st, buffer2=h2)
        call check(error, r, -3_int64)
        if (allocated(error)) return
        call compare_buffer_substrings(r, st, start1=3_int64, end1=1_int64, buffer2=h2, start2=1_int64, end2=3_int64)
        call check(error, r, 0_int64)
        if (allocated(error)) return
        call compare_buffer_substrings(r, st, end1=3_int64, buffer2=h2)
        call check(error, r, -3_int64)
        if (allocated(error)) return
        call compare_buffer_substrings(r, st, buffer2=h2, end2=3_int64)
        call check(error, r, 3_int64)
        if (allocated(error)) return
        call compare_buffer_substrings(r, st, end1=9_int64)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE .and. st%detail == "1, 9", "out of range")
        case_fold_search = saved
        if (allocated(error)) return
        call close_buffer(h2, st)
        call compare_buffer_substrings(r, st, buffer2=h2)
        call check(error, format_error(st), "Selecting deleted buffer")
        call leave(h, old2)
    end subroutine test_compare_substrings

    !> char-to-string and string-to-char.
    subroutine test_char_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: s

        call char_to_string(8364, s, st)
        call check(error, st%ok() .and. s == euro, "euro")
        if (allocated(error)) return
        call char_to_string(-1, s, st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call check(error, string_to_char(euro//"x") == 8364 .and. string_to_char("") == 0, "string-to-char")
    end subroutine test_char_string

    !> char-equal ignores case when case-fold-search is true.
    subroutine test_char_equal(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: saved, a, b, c

        saved = case_fold_search%value
        case_fold_search = .true.
        a = is_char_equal(iachar("a"), iachar("A"), st)
        b = is_char_equal(iachar("a"), iachar("b"), st)
        case_fold_search = .false.
        c = is_char_equal(iachar("a"), iachar("A"), st)
        case_fold_search = saved
        call check(error, a .and. .not. b .and. .not. c, "char-equal")
        if (allocated(error)) return
        a = is_char_equal(-5, 65, st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
    end subroutine test_char_equal

    !> ngettext, emacs-pid and group-real-gid.
    subroutine test_ngettext_ids(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, ngettext("file", "files", 1_int64) == "file" .and. ngettext("file", "files", 2_int64) == "files" &
                   .and. ngettext("file", "files", 0_int64) == "files", "ngettext")
        if (allocated(error)) return
        call check(error, emacs_pid() == emacs_getpid() .and. emacs_pid() > 0, "emacs-pid")
        if (allocated(error)) return
        call check(error, group_real_gid() == emacs_getgid(), "group-real-gid")
    end subroutine test_ngettext_ids

    !> current-message gives the text the echo area shows, unallocated
    !> when it is empty.
    subroutine test_current_message(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text

        call message("te-current-message")
        call current_message(text)
        call check(error, allocated(text), "allocated")
        if (allocated(error)) return
        call check(error, text, "te-current-message")
        if (allocated(error)) return
        call message("")
        call current_message(text)
        call check(error,.not. allocated(text), "cleared")
    end subroutine test_current_message

    !> editfns-tests.el test-group-name: the name of the effective gid is
    !> a string or unallocated, and for 0, 1212345 and that gid the name
    !> is the one `getent group GID' prints, or unallocated when it finds
    !> none (status 2).  (The non-number argument has no Fortran form.)
    subroutine test_group_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(value_type) :: r
        character(len=:), allocatable :: name, text
        character(len=24) :: number
        integer(int64) :: gids(3)
        integer :: k, colon

        call group_name(int(group_gid(), int64), name, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call group_name(-1_int64, name, st)
        call check(error,.not. st%ok(), "an out-of-range gid")
        if (allocated(error)) return
        gids = [0_int64, 1212345_int64, int(group_gid(), int64)]
        call enter("te-group-name", h, old)

        do k = 1, size(gids)
            call erase_buffer(st)
            write (number, "(i0)") gids(k)
            call execute_process("getent", r, st, destination=value_type(.true.), &
                                 args=list(value_type(text="group"), value_type(text=trim(number))))
            ! No getent: nothing to compare with.
            if (.not. st%ok()) exit
            if (vtag(r) /= V_INTEGER) cycle
            call group_name(gids(k), name, st)

            if (xfixnum(r) == 0) then
                text = buffer_string()
                colon = index(text, ":")

                if (colon > 1) then
                    call check(error, allocated(name), "a name for "//trim(number))
                    if (allocated(error)) exit
                    call check(error, name, text(1:colon - 1))
                    if (allocated(error)) exit
                end if
            else if (xfixnum(r) == 2) then
                call check(error,.not. allocated(name), "no name for "//trim(number))
                if (allocated(error)) exit
            end if
        end do

        call leave(h, old)
    end subroutine test_group_name

    ! ------------------------------------------------------------------
    ! Transposing, translating, excursions and restrictions
    ! ------------------------------------------------------------------

    !> Reverse the characters from START to END by transposing pairs of
    !> characters, leaving the markers.  Port of
    !> editfns-tests.el:transpose-test-reverse-word.
    subroutine reverse_word(start, end)
        integer(int64), intent(in) :: start, end
        type(handle_type) :: begm, endm
        type(status_type) :: st
        integer(int64) :: b, e

        call make_marker(begm)
        call set_marker(begm, start, st)
        call make_marker(endm)
        call set_marker(endm, end, st)

        do
            call marker_position(begm, b, st)
            call marker_position(endm, e, st)
            if (e <= b) exit
            call transpose_regions(b, b + 1, e, e + 1, st, leave_markers=.true.)
            call set_marker(begm, b + 1, st)
            call set_marker(endm, e - 1, st)
        end do

        call destroy_marker(begm, st)
        call destroy_marker(endm, st)
    end subroutine reverse_word

    !> Whether the byte positions of the characters 1 to size(expected)
    !> are EXPECTED.  Port of editfns-tests.el:transpose-test-get-byte-positions.
    logical function has_byte_positions(expected)
        integer(int64), intent(in) :: expected(:)
        integer(int64) :: k, p
        logical :: found

        has_byte_positions = .true.

        do k = 1, size(expected, kind=int64)
            call position_bytes(k, p, found)
            has_byte_positions = has_byte_positions .and. found .and. p == expected(k)
        end do
    end function has_byte_positions

    !> Reverse TEXT's characters FROM..TO and expect RESULT with byte
    !> positions BYTES.
    subroutine check_reverse(error, name, text, from, to, result, bytes)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, text, result
        integer(int64), intent(in) :: from, to, bytes(:)
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter(name, h, old)
        call insert(text, st)
        call reverse_word(from, to)
        call check(error, buffer_string(), result)
        if (allocated(error)) return
        call check(error, has_byte_positions(bytes), "byte positions")
        call leave(h, old)
    end subroutine check_reverse

    ! editfns-tests.el: transpose-ascii-regions-test
    subroutine test_transpose_ascii(error)
        type(error_type), allocatable, intent(out) :: error

        call check_reverse(error, "te-tr-ascii", "abcd", 1_int64, 4_int64, "dcba", [1_int64, 2_int64, 3_int64, 4_int64, 5_int64])
    end subroutine test_transpose_ascii

    ! editfns-tests.el: transpose-nonascii-regions-test-1
    subroutine test_transpose_nonascii_1(error)
        type(error_type), allocatable, intent(out) :: error

        call check_reverse(error, "te-tr-na1", "÷bcd", 1_int64, 4_int64, "dcb÷", [1_int64, 2_int64, 3_int64, 4_int64, 6_int64])
    end subroutine test_transpose_nonascii_1

    ! editfns-tests.el: transpose-nonascii-regions-test-2
    subroutine test_transpose_nonascii_2(error)
        type(error_type), allocatable, intent(out) :: error

        call check_reverse(error, "te-tr-na2", "÷ab""äé", 1_int64, 6_int64, "éä""ba÷", &
                           [1_int64, 3_int64, 5_int64, 6_int64, 7_int64, 8_int64, 10_int64])
    end subroutine test_transpose_nonascii_2

    !> The `my_prop' (or PROP) property of each character of the accessible
    !> text, its name or "" for none.
    function names_of_property(prop) result(names)
        character(len=*), intent(in) :: prop
        character(len=8), allocatable :: names(:)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p

        allocate (names(point_max() - point_min()))

        do p = point_min(), point_max() - 1
            call get_text_property(p, value_type(name=prop), v, st)
            names(p - point_min() + 1) = ""
            if (vtag(v) == V_NAME) names(p - point_min() + 1) = xsymbol_name(v)
        end do
    end function names_of_property

    !> The name a name value holds.
    function xsymbol_name(v) result(name)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: name

        name = symbols(xsymbol(v))%name
    end function xsymbol_name

    ! editfns-tests.el: editfns-tests--transpose-equal-but-not
    subroutine test_transpose_equal_but_not(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=8), allocatable :: names(:)
        logical :: changed

        call enter("te-tr-equal", h, old)
        call insert(" ab", st)
        call add_text_properties(2_int64, 4_int64, list(value_type(name="my_prop"), value_type(name="ab")), changed, st)
        call insert("SPC", st)
        call add_text_properties(4_int64, 7_int64, list(value_type(name="my_prop"), value_type(name="SPC")), changed, st)
        call insert("é", st)
        call add_text_properties(7_int64, 8_int64, list(value_type(name="my_prop"), value_type(name="é")), changed, st)
        call insert(" ", st)
        call transpose_regions(point_min() + 1, point_min() + 3, point_min() + 6, point_min() + 7, st)
        call check(error, buffer_string(), " éSPCab ")
        if (allocated(error)) return
        names = names_of_property("my_prop")
        call check(error, all(names == [character(len=8) :: "", "é", "SPC", "SPC", "SPC", "ab", "ab", ""]), &
                   "properties move with the text")
        call leave(h, old)
    end subroutine test_transpose_equal_but_not

    !> Find the markup BE in TEXT as the regular expression "B.+E" would:
    !> the first B with an E after at least one character on its line, the
    !> last such E.  IB and IE are their bytes (0 when there is none).
    subroutine find_markup(text, b, e, ib, ie)
        character(len=*), intent(in) :: text, b, e
        integer, intent(out) :: ib, ie
        integer :: i, line_end, j

        ib = 0
        ie = 0

        do i = 1, len(text)
            if (text(i:i) /= b) cycle
            line_end = index(text(i + 1:), new_line("a"))

            if (line_end == 0) then
                line_end = len(text)
            else
                line_end = i + line_end - 1
            end if

            if (line_end < i + 2) cycle
            j = index(text(i + 2:line_end), e, back=.true.)

            if (j > 0) then
                ib = i
                ie = i + 1 + j
                return
            end if
        end do
    end subroutine find_markup

    ! editfns-tests.el: editfns-tests--transpose-regions (the undo is
    ! checked on the text: undo records keep no text properties yet)
    subroutine test_transpose_regions(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: ntests = 19
        character(len=40) :: tests(5, ntests)
        integer(int64) :: blengthv(5, ntests)
        integer :: t, l

        tests(:, 1) = [character(len=40) :: "$", "foo$", "$", "$", "$"]
        blengthv(:, 1) = [0, 3, 0, 0, 0]
        tests(:, 2) = [character(len=40) :: "$", "$", "$", "baz$", "$"]
        blengthv(:, 2) = [0, 0, 0, 3, 0]
        tests(:, 3) = [character(len=40) :: "$", "fo(o$", "$", "b)az$", "$"]
        blengthv(:, 3) = [0, 3, 0, 3, 0]
        tests(:, 4) = [character(len=40) :: "$", "fo(o$", "[bar]$", "b)az$", "$"]
        blengthv(:, 4) = [0, 3, 3, 3, 0]
        tests(:, 5) = [character(len=40) :: "$", "fo(o$", "[bar]$", "baaz)$", "$"]
        blengthv(:, 5) = [0, 3, 3, 4, 0]
        tests(:, 6) = [character(len=40) :: "$", "(fooo$", "[bar]$", "baz)$", "$"]
        blengthv(:, 6) = [0, 4, 3, 3, 0]
        tests(:, 7) = [character(len=40) :: "$", "fo(o$", "$", "b)äz$", "$"]
        blengthv(:, 7) = [0, 3, 0, 4, 0]
        tests(:, 8) = [character(len=40) :: "$", "fo(ö$", "$", "b)az$", "$"]
        blengthv(:, 8) = [0, 4, 0, 3, 0]
        tests(:, 9) = [character(len=40) :: "$", "fo(o$", "[bar]$", "b)äz$", "$"]
        blengthv(:, 9) = [0, 3, 3, 4, 0]
        tests(:, 10) = [character(len=40) :: "$", "fo(ö$", "[bar]$", "b)az$", "$"]
        blengthv(:, 10) = [0, 4, 3, 3, 0]
        tests(:, 11) = [character(len=40) :: "$", "fo(ö$", "$", "baaz)$", "$"]
        blengthv(:, 11) = [0, 4, 0, 4, 0]
        tests(:, 12) = [character(len=40) :: "$", "(fooo$", "$", "bäz)$", "$"]
        blengthv(:, 12) = [0, 4, 0, 4, 0]
        tests(:, 13) = [character(len=40) :: "$", "fo(ö$", "[bar]$", "baaz)$", "$"]
        blengthv(:, 13) = [0, 4, 3, 4, 0]
        tests(:, 14) = [character(len=40) :: "$", "(fooo$", "[bar]$", "bäz)$", "$"]
        blengthv(:, 14) = [0, 4, 3, 4, 0]
        tests(:, 15) = [character(len=40) :: "ἀ(ρχή$", "φ[ωω$", "β){αρ$", "β<ἀ]ζ}$", "τέλ>ος$"]
        blengthv(:, 15) = [9, 6, 6, 7, 10]
        tests(:, 16) = [character(len=40) :: "ἀ(ρχή$", "φ[ωω$", "β){αρ$", "β<α]ζ}$", "τέλ>ος$"]
        blengthv(:, 16) = [9, 6, 6, 6, 10]
        tests(:, 17) = [character(len=40) :: " $", "(ab)$", "[SPC]$", "{é}$", " $"]
        blengthv(:, 17) = [1, 2, 3, 2, 1]
        tests(:, 18) = [character(len=40) :: "$", "$", "(a):"//new_line("a")//"[b]: ℓ₀"//new_line("a")//"$", &
                        "{v}: scaling$", "$"]
        blengthv(:, 18) = [0, 0, 13, 10, 0]
        tests(:, 19) = [character(len=40) :: "(Query replace (default abc → d): )$", "abc$", "[ → ]$", "d$", "$"]
        blengthv(:, 19) = [35, 3, 5, 1, 0]

        do t = 1, ntests
            do l = 0, 1
                call check_transposition(error, t, tests(:, t), blengthv(:, t), l == 1)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_transpose_regions

    !> One case of editfns-tests--transpose-regions: the snippets TEST
    !> (HEAD REG1 MID REG2 TAIL, each ended by a "$" that is not inserted) with byte lengths
    !> BLENGTHV after the markup is removed, transposed with LEAVE_MARKERS.
    subroutine check_transposition(error, t, test, blengthv, leave_markers)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: t
        character(len=*), intent(in) :: test(5)
        integer(int64), intent(in) :: blengthv(5)
        logical, intent(in) :: leave_markers
        character(len=2), parameter :: markups(4) = ["()", "[]", "{}", "<>"]
        type(handle_type) :: h, old, smarkers(4), pmarkers(8)
        type(status_type) :: st
        integer(int64) :: bounds(0:5), pmpos(8), p, b, e, pb, pe, start1, end1, start2, end2
        integer :: k, npm, ib, ie
        character(len=8), allocatable :: names(:), tnames(:)
        character(len=:), allocatable :: text, original, transposed
        character(len=8) :: label
        logical :: changed, found, test_undo

        write (label, "(i0, a, l1)") t, " ", leave_markers
        call enter("te-tr-table", h, old)

        ! Insert text snippets, with the separator markers.
        do k = 1, 4
            call insert(test(k) (1:len_trim(test(k)) - 1), st)
            call point_marker(smarkers(k))
        end do

        call insert(test(5) (1:len_trim(test(5)) - 1), st)
        ! Propertize them according to markup, remove markup characters,
        ! add property markers.
        npm = 0

        do k = 1, 4
            text = buffer_string()
            call find_markup(text, markups(k) (1:1), markups(k) (2:2), ib, ie)
            if (ib == 0) cycle
            call byte_to_position(int(ib, int64), b, found)
            call byte_to_position(int(ie, int64), e, found)
            call copy_marker_at(b, pmarkers(npm + 1), .false., st)
            call copy_marker_at(e + 1, pmarkers(npm + 2), .false., st)
            call marker_position(pmarkers(npm + 1), b, st)
            call delete_region(b, b + 1, st)
            call marker_position(pmarkers(npm + 2), e, st)
            call delete_region(e - 1, e, st)
            call marker_position(pmarkers(npm + 1), b, st)
            call marker_position(pmarkers(npm + 2), e, st)
            call add_text_properties(b, e, list(value_type(name="markup"), value_type(name=markups(k))), changed, st)
            npm = npm + 2
        end do

        do k = 1, npm
            call marker_position(pmarkers(k), pmpos(k), st)
        end do

        ! The net text snippets and their byte lengths.
        bounds(0) = point_min()
        bounds(5) = point_max()

        do k = 1, 4
            call marker_position(smarkers(k), bounds(k), st)
        end do

        do k = 1, 5
            call position_bytes(bounds(k), pe, found)
            call position_bytes(bounds(k - 1), pb, found)
            call check(error, pe - pb, blengthv(k), "byte length of snippet, case "//label)
            if (allocated(error)) return
        end do

        original = buffer_string()
        names = names_of_property("markup")
        ! The texts and properties with REG1 and REG2 transposed.
        transposed = substring(bounds(0), bounds(1))//substring(bounds(3), bounds(4))//substring(bounds(2), bounds(3))// &
                     substring(bounds(1), bounds(2))//substring(bounds(4), bounds(5))
        ! Filled by sections, not an array constructor: a constructor whose
        ! first parts are empty reallocates to size 0 (PORTING.md, Hazards).
        allocate (tnames(size(names)))
        p = 1
        tnames(p:p + bounds(1) - bounds(0) - 1) = names(bounds(0):bounds(1) - 1)
        p = p + bounds(1) - bounds(0)
        tnames(p:p + bounds(4) - bounds(3) - 1) = names(bounds(3):bounds(4) - 1)
        p = p + bounds(4) - bounds(3)
        tnames(p:p + bounds(3) - bounds(2) - 1) = names(bounds(2):bounds(3) - 1)
        p = p + bounds(3) - bounds(2)
        tnames(p:p + bounds(2) - bounds(1) - 1) = names(bounds(1):bounds(2) - 1)
        p = p + bounds(2) - bounds(1)
        tnames(p:p + bounds(5) - bounds(4) - 1) = names(bounds(4):bounds(5) - 1)
        start1 = bounds(1)
        end1 = bounds(2)
        start2 = bounds(3)
        end2 = bounds(4)
        call undo_boundary()
        call transpose_regions(start1, end1, start2, end2, st, leave_markers)
        test_undo = undo_last_kind() /= UE_BOUNDARY
        call undo_boundary()
        call check(error, st%ok() .and. buffer_string() == transposed, "text, case "//label)
        if (allocated(error)) return
        call check(error, all(names_of_property("markup") == tnames), "properties, case "//label)
        if (allocated(error)) return

        ! The property markers.
        if (.not. leave_markers) then
            do k = 1, npm
                p = pmpos(k)

                if (p >= start1 .and. p < end2) then
                    if (p < end1) then
                        pmpos(k) = p + (end2 - start2) + (start2 - end1)
                    else if (p < start2) then
                        pmpos(k) = p + (end2 - start2) - (end1 - start1)
                    else
                        pmpos(k) = p - ((end1 - start1) + (start2 - end1))
                    end if
                end if
            end do
        end if

        do k = 1, npm
            call marker_position(pmarkers(k), p, st)
            call check(error, p, pmpos(k), "property marker, case "//label)
            if (allocated(error)) return
        end do

        ! Undo the transposition and check the text again.
        if (test_undo) then
            last_command => null()
            call undo(prefix_arg_type(), st)
            call check(error, buffer_string(), original, "undone, case "//label)
            if (allocated(error)) return
        end if

        do k = 1, 4
            call destroy_marker(smarkers(k), st)
        end do

        do k = 1, npm
            call destroy_marker(pmarkers(k), st)
        end do

        call leave(h, old)

    contains

        !> The text from A to B.
        function substring(a, b) result(s)
            integer(int64), intent(in) :: a, b
            character(len=:), allocatable :: s
            type(status_type) :: st2

            call buffer_substring(a, b, s, st2)
        end function substring
    end subroutine check_transposition

    !> Overlapping regions are an error; the regions may come in either order.
    subroutine test_transpose_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-tr-errors", h, old)
        call insert("abcdef", st)
        call transpose_regions(1_int64, 4_int64, 3_int64, 5_int64, st)
        call check(error, format_error(st), "Transposed regions overlap")
        if (allocated(error)) return
        call transpose_regions(5_int64, 7_int64, 3_int64, 1_int64, st)
        call check(error, st%ok() .and. buffer_string() == "efcdab", buffer_string())
        if (allocated(error)) return
        call transpose_regions(1_int64, 3_int64, 4_int64, 9_int64, st)
        call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
        call leave(h, old)
    end subroutine test_transpose_errors

    !> A before-change function that moves the gap with silent
    !> modifications (editfns-tests--bug76124's).
    subroutine bug76124_before(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(silent_modifications_type) :: silent

        status = status_type()
        call save_excursion_begin(excursion)
        call goto_char(beginning)
        call end_of_line()

        if (.not. point() > end) then
            call silent_modifications_begin(silent)
            call insert("foo", status)
            if (status%ok()) call delete_char(-3_int64, status)
            call silent_modifications_end(silent)
        end if

        call save_excursion_end(excursion)
    end subroutine bug76124_before

    ! editfns-tests.el: editfns-tests--bug76124
    subroutine test_bug76124(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("te-bug76124", h, old)
        call insert("Emacs forever!foo"//new_line("a"), st)
        call insert("toto"//new_line("a"), st)
        call goto_char(point_min())
        ! Remove the trailing "foo", so as to move the gap between the
        ! two lines.
        call delete_region(pos_eol() - 3, pos_eol(), st)
        call before_change_functions%prepend(bug76124_before, local=.true.)
        call goto_char(point_min())
        call transpose_regions(pos_bol(), pos_eol(), pos_bol(2_int64), pos_eol(2_int64), st)
        call before_change_functions%clear_local()
        call check(error, st%ok() .and. buffer_string() == "toto"//new_line("a")//"Emacs forever!"//new_line("a"), &
                   buffer_string())
        call leave(h, old)
    end subroutine test_bug76124

    ! editfns-tests.el: test-translate-region-internal
    subroutine test_translate_region_internal(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, tt
        type(status_type) :: st
        integer(int64) :: changed

        call enter("te-translate", h, old)
        call make_char_table(tt, "translation_table", st)
        call char_table_set(tt, int(MAX_CHAR), int(iachar("*"), int64), st)
        call insert_char_n(MAX_CHAR, 1_int64, st)
        call translate_region_internal(point_min(), point_max(), tt, changed, st)
        call check(error, st%ok() .and. buffer_string() == "*", buffer_string())
        if (allocated(error)) return
        call check(error, changed, 1_int64)
        call destroy_char_table(tt, st)
        call leave(h, old)
    end subroutine test_translate_region_internal

    !> A string table maps code N to its Nth character; other tables are
    !> refused.
    subroutine test_translate_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, ct
        type(status_type) :: st
        integer(int64) :: changed
        character(len=128) :: table
        integer :: k

        call enter("te-translate-string", h, old)

        do k = 0, 127
            table(k + 1:k + 1) = achar(k)
        end do

        table(iachar("a") + 1:iachar("a") + 1) = "b"
        call insert("banana", st)
        call translate_region_internal(2_int64, 6_int64, table, changed, st)
        call check(error, st%ok() .and. buffer_string() == "bbnbna", buffer_string())
        if (allocated(error)) return
        call check(error, changed, 2_int64)
        if (allocated(error)) return
        call make_char_table(ct, "syntax_table", st)
        call translate_region_internal(1_int64, 3_int64, ct, changed, st)
        call check(error, format_error(st), "Not a translation table")
        call destroy_char_table(ct, st)
        call leave(h, old)
    end subroutine test_translate_string

    !> save-excursion restores the buffer and point, also after the text
    !> before point changed.
    subroutine test_save_excursion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, other, old2
        type(status_type) :: st
        type(excursion_type) :: excursion

        call enter("te-excursion", h, old)
        call insert("abcdef", st)
        call goto_char(4_int64)
        call save_excursion_begin(excursion)
        call goto_char(1_int64)
        call insert("XY", st)
        call enter("te-excursion-2", other, old2)
        call save_excursion_end(excursion)
        call check(error, same(current_buffer(), h) .and. point() == 6, "back in the buffer, after the insertion")
        if (allocated(error)) return
        call set_buffer(other, st)
        call leave(other, h)
        call leave(h, old)

    contains

        logical function same(a, b)
            type(handle_type), intent(in) :: a, b

            same = a%index == b%index .and. a%generation == b%generation
        end function same
    end subroutine test_save_excursion

    !> save-restriction restores narrowing and its absence; the saved end
    !> moves with insertions at it.
    subroutine test_save_restriction(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        type(restriction_type) :: restriction

        call enter("te-restriction", h, old)
        call insert("0123456789", st)
        call save_restriction_begin(restriction)
        call narrow_to_region(3_int64, 5_int64, st)
        call save_restriction_end(restriction)
        call check(error, point_min() == 1 .and. point_max() == 11, "widened again")
        if (allocated(error)) return
        call narrow_to_region(3_int64, 6_int64, st)
        call save_restriction_begin(restriction)
        call widen()
        call goto_char(6_int64)
        call insert("XY", st)
        call goto_char(11_int64)
        call save_restriction_end(restriction)
        call check(error, point_min() == 3 .and. point_max() == 8, "narrowed again, the end moved")
        if (allocated(error)) return
        call check(error, point(), 8_int64)
        call leave(h, old)
    end subroutine test_save_restriction

    !> Labeled restrictions: narrow_to_region stays within them, widen
    !> goes back to them, the labeled widen removes them, save-restriction
    !> keeps them, redisplay shows the outermost bounds.
    subroutine test_labeled_restrictions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old, pm
        type(handle_type), allocatable :: restricted(:)
        type(status_type) :: st
        type(restriction_type) :: restriction

        call enter("te-labeled", h, old)
        call insert("0123456789", st)
        call narrow_to_region(2_int64, 10_int64, st)
        call internal_labeled_narrow_to_region(4_int64, 8_int64, "test", st)
        call check(error, st%ok() .and. point_min() == 4 .and. point_max() == 8, "labeled narrowing")
        if (allocated(error)) return
        call narrow_to_region(1_int64, 6_int64, st)
        call check(error, point_min() == 4 .and. point_max() == 6, "narrowing within the labeled one")
        if (allocated(error)) return
        call widen()
        call check(error, point_min() == 4 .and. point_max() == 8, "widen to the labeled restriction")
        if (allocated(error)) return
        call save_restriction_begin(restriction)
        call internal_labeled_widen("other")
        call check(error, point_min() == 4 .and. point_max() == 8, "another label leaves it")
        if (allocated(error)) return
        call internal_labeled_widen("test")
        call check(error, point_min() == 2 .and. point_max() == 10, "the label removes it")
        if (allocated(error)) return
        call widen()
        call check(error, point_min() == 1 .and. point_max() == 11, "then widen widens")
        if (allocated(error)) return
        call save_restriction_end(restriction)
        call check(error, point_min() == 4 .and. point_max() == 8, "save-restriction restores it")
        if (allocated(error)) return
        call reset_outermost_restrictions_begin(restricted)
        call check(error, size(restricted) == 1 .and. point_min() == 2 .and. point_max() == 10, "the outermost bounds")
        if (allocated(error)) return
        call reset_outermost_restrictions_end(restricted)
        call check(error, point_min() == 4 .and. point_max() == 8, "the labeled bounds again")
        if (allocated(error)) return
        call goto_char(5_int64)
        call labeled_narrow_to_region_begin(5_int64, 7_int64, "inner", pm, st)
        call goto_char(7_int64)
        call check(error, point_min() == 5 .and. point_max() == 7, "labeled_narrow_to_region")
        if (allocated(error)) return
        call labeled_narrow_to_region_end("inner", pm)
        call check(error, point_min() == 4 .and. point_max() == 8 .and. point() == 5, "and its end")
        if (allocated(error)) return
        call labeled_restrictions_remove_in_current_buffer()
        call widen()
        call check(error, point_min() == 1 .and. point_max() == 11, "all removed")
        call leave(h, old)
    end subroutine test_labeled_restrictions
end module test_editfns_mod
