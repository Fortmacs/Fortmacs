! test_editfns_fm.f90 -- tests for editfns_fm.
! Own tests; editfns-tests.el's cases need `format', text properties,
! undo or `transpose-regions'.
!
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

!> The editfns-tests.el cases need `format`, text properties, undo or
!> `transpose-regions`, none of which exist yet; these are our own.
module test_editfns_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_ARGS_OUT_OF_RANGE, FM_GENERIC_ERROR, format_error, format_message
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, kill_buffer, set_buffer, &
                         current_buffer, is_buffer_live, erase_buffer
    use marker_fm, only: make_marker, set_marker, free_marker, marker_position
    use insdel_fm, only: insert
    use editfns_fm, only: is_bob, is_bol, buffer_size, buffer_size_of, buffer_string, buffer_substring, &
                          byte_to_position, char_after, char_before, delete_and_extract_region, &
                          delete_region, is_eob, is_eol, following_char, gap_position, goto_char, &
                          goto_char_marker, insert_char_n, narrow_to_region, point, point_marker, &
                          point_max, point_max_marker, point_min, point_min_marker, position_bytes, &
                          preceding_char, widen, field_beginning, field_end, field_string, &
                          constrain_to_field, line_beginning_position, line_end_position, &
                          delete_field, subst_char_in_region, interactive_region, check_mark, mark_marker
    use buffer_fm, only: buffers, cur, transient_mark_mode
    use callint_fm, only: mark_even_if_inactive
    use error_fm, only: FM_MARK_INACTIVE
    use lisp_fm, only: list, t_value
    use lread_fm, only: sym
    use textprop_fm, only: add_text_properties, put_text_property, get_text_property
    use lisp_fm, only: value_type
    use data_fm, only: value_eq
    implicit none(type, external)
    private

    public :: collect_editfns_fm

    character(len=*), parameter :: euro = char(226)//char(130)//char(172)

contains

    !> Collect all exported unit tests.
    subroutine collect_editfns_fm(testsuite)
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
                    new_unittest("interactive-region", test_interactive_region) &
                    ]
    end subroutine collect_editfns_fm

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
        call free_marker(m, st)
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
        call put_text_property(2_int64, 3_int64, sym("face"), sym("bold"), st)
        call goto_char(3_int64)
        call subst_char_in_region(6_int64, 1_int64, 9, 32, st)
        call check(error, st%ok() .and. buffer_string() == "a b c" .and. point() == 3, "subst: "//buffer_string())
        if (allocated(error)) return
        call get_text_property(2_int64, sym("face"), v, st)
        call check(error, value_eq(v, sym("bold")), "property kept")
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
        call free_marker(pm, st)
        call free_marker(lo, st)
        call free_marker(hi, st)
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
        call add_text_properties(4_int64, 7_int64, list(sym("field"), sym("bar")), changed, st)
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
        call check(error, field_string(5_int64) == "bar")
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
                                 list(sym("field"), t_value(), sym("front_sticky"), t_value(), &
                                      sym("rear_nonsticky"), t_value()), changed, st)
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
        buffers(cur)%mark_active = .true.
        call narrow_to_region(4_int64, 8_int64, st)
        call goto_char(6_int64)
        call interactive_region(b, e, st)
        call check(error, st%ok() .and. b == 2 .and. e == 6, "the mark outside the narrowing")
        if (allocated(error)) return
        saved = mark_even_if_inactive
        buffers(cur)%mark_active = .false.
        mark_even_if_inactive = .false.
        call interactive_region(b, e, st)
        mark_even_if_inactive = saved
        call check(error, st%code == FM_MARK_INACTIVE .and. transient_mark_mode%value, "inactive mark")
        call leave(h, old)
    end subroutine test_interactive_region
end module test_editfns_fm
