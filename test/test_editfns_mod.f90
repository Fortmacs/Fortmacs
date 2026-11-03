! test_editfns_mod.f90 -- tests for editfns_mod.
! Ports GNU Emacs 31.1 test/src/editfns-tests.el in part; the rest need
! `format', undo marker adjustments, save-restriction or
! `transpose-regions' (the change-hook cases are in test_insdel_mod).
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

!> Ports find-fields and test-group-name; the other tests are our own.
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
                    new_unittest("test-group-name", test_group_name) &
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
end module test_editfns_mod
