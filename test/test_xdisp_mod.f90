! test_xdisp_mod.f90 -- tests for xdisp_mod, window_mod and emacs_mod: how
! buffer text is laid out on the screen, and the editor's main loop on a
! pty.
! Own tests; upstream's xdisp-tests.el needs Lisp and GUI frames.
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

module test_xdisp_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, resolve_buffer, &
                          erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, buffer_string
    use dispextern_mod, only: glyph_row_type, glyph_type, DEFAULT_FACE_ID
    use faces_mod, only: init_faces
    use xfaces_mod, only: init_frame_faces
    use tty_colors_mod, only: tty_color_clear, tty_register_colors
    use terminfo_mod, only: terminfo_type, builtin_xterm
    use term_mod, only: init_tty_entry
    use termchar_mod, only: tty_take_output
    use frame_mod, only: frame_type
    use window_mod, only: delete_window_tree, delete_window_object, split_window_internal, set_window_buffer, windows
    use xdisp_mod, only: layout_window, redisplay_internal, mode_line_text, glyph_width, mode_line_row, init_frame_windows, &
                         mode_line_percent, recenter_start, message, mode_line_modes
    use keyboard_mod, only: last_message, set_minibuffer_message_text
    use frame_mod, only: selected_frame, ensure_frame_windows
    use handle_mod, only: null_handle
    use editfns_mod, only: narrow_to_region, widen
    use sysdep_mod, only: open_pty, close_fd, set_tty_size, init_sys_modes, &
                          reset_sys_modes, emacs_write, emacs_read, input_available, &
                          emacs_tty_type
    use emacs_mod, only: emacs_main
    use value_mod, only: list, value_type, make_window_ref
    use textprop_mod, only: string_put_text_property
    use value_mod, only: value_type
    use textprop_mod, only: put_text_property
    use itree_mod, only: make_overlay, overlay_put, destroy_overlay
    use subr_mod, only: add_to_invisibility_spec
    implicit none(type, external)
    private

    public :: collect_xdisp_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_xdisp_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("layout-lines-and-cursor", test_lines), &
                    new_unittest("layout-tab-control-wide", test_special_chars), &
                    new_unittest("layout-continuation", test_continuation), &
                    new_unittest("layout-wide-at-right-edge", test_wide_edge), &
                    new_unittest("layout-split-control-and-tab", test_split_chars), &
                    new_unittest("recenter-by-screen-lines", test_recenter_start), &
                    new_unittest("mode-line-percent-and-narrow", test_mode_line_position), &
                    new_unittest("echo-area-glyphs", test_echo_area), &
                    new_unittest("message-in-active-minibuffer", test_minibuffer_message), &
                    new_unittest("layout-end-of-buffer", test_end_of_buffer), &
                    new_unittest("layout-region-face", test_region), &
                    new_unittest("mode-line-text", test_mode_line), &
                    new_unittest("redisplay-recenters", test_redisplay), &
                    new_unittest("redisplay-split-windows", test_split_redisplay), &
                    new_unittest("overlay-strings", test_overlay_strings), &
                    new_unittest("overlay-string-properties", test_overlay_string_properties), &
                    new_unittest("invisible-ellipsis", test_invisible_ellipsis), &
                    new_unittest("invisible-text", test_invisible), &
                    new_unittest("invisible-newline", test_invisible_newline), &
                    new_unittest("display-string", test_display_string), &
                    new_unittest("display-space-align-to", test_display_space), &
                    new_unittest("header-line", test_header_line), &
                    new_unittest("emacs-main-on-a-pty", test_emacs_main) &
                    ]
    end subroutine collect_xdisp_mod

    !> A buffer holding `text`, current; its slot is `bi`.
    subroutine buffer_with(name, text, h, bi)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        integer(int64), intent(out) :: bi
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call resolve_buffer(h, bi, st)
    end subroutine buffer_with

    !> The text of a laid-out row (wide-character padding left out).
    function row_text(row) result(s)
        type(glyph_row_type), intent(in) :: row
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, row%used
            if (row%glyphs(k)%padding) cycle

            if (row%glyphs(k)%ch < 128) then
                s = s//achar(row%glyphs(k)%ch)
            else
                s = s//"?"
            end if
        end do
    end function row_text

    subroutine layout(bi, start, pt, width, nrows, rows, crow, ccol, end_pos, at_end)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width, nrows
        type(glyph_row_type), allocatable, intent(out) :: rows(:)
        integer, intent(out) :: crow, ccol
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: at_end
        integer :: r

        allocate (rows(nrows))

        do r = 1, nrows
            allocate (rows(r)%glyphs(width))
        end do

        call layout_window(bi, start, pt, width, rows, crow, ccol, end_pos, at_end)
    end subroutine layout

    !> The text, cursor row and column of one-row layout of buffer BI with
    !> point at PT, 30 columns wide.
    subroutine layout1(bi, pt, text, ccol)
        integer(int64), intent(in) :: bi, pt
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ccol
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: end_pos
        integer :: crow
        logical :: at_end

        call layout(bi, 1_int64, pt, 30, 2, rows, crow, ccol, end_pos, at_end)
        text = row_text(rows(1))
        if (crow /= 0) ccol = -1
    end subroutine layout1

    !> Before-strings at an overlay's start, after-strings at its end,
    !> and the cursor on the buffer text after them (as GNU Emacs shows
    !> them).
    subroutine test_overlay_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, ov, ov2
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-ov*", "abcdef", h, bi)
        call make_overlay(ov, 3_int64, 5_int64, st)
        call overlay_put(ov, "before_string", value_type(text="BS"), st)
        call overlay_put(ov, "after_string", value_type(text="AS"), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "abBScdASef", text)
        if (allocated(error)) return
        call check(error, ccol == 4, "cursor on c")
        if (allocated(error)) return
        call destroy_overlay(ov)
        ! An after-string of an empty overlay at point.
        call make_overlay(ov2, 3_int64, 3_int64, st)
        call overlay_put(ov2, "after_string", value_type(text="AS"), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "abAScdef", text)
        if (allocated(error)) return
        call check(error, ccol == 4, "cursor after the string")
        call destroy_overlay(ov2)
        call close_buffer(h, st)
    end subroutine test_overlay_strings

    !> An after-string with text properties: a `(space :align-to N)'
    !> display spec, its `face' and its `cursor'; and an overlay for
    !> another window, which is not shown.
    subroutine test_overlay_string_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, ov, ov2
        type(status_type) :: st
        type(value_type) :: string
        type(glyph_row_type) :: rows(2)
        type(frame_type) :: f
        integer(int64) :: bi, end_pos
        integer(int32) :: region_id
        character(len=:), allocatable :: text
        integer :: ccol, crow, r
        logical :: at_end

        call buffer_with("*xdisp-ov-props*", "ab"//achar(10)//"cd", h, bi)
        string = value_type(text=" ")
        call string_put_text_property(0_int64, 1_int64, value_type(name="display"), &
                                      list(value_type(name="space"), value_type(name=":align-to"), value_type(5_int64)), string, st)
        call make_overlay(ov, 3_int64, 3_int64, st)
        call overlay_put(ov, "after_string", string, st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "ab   ", text)
        if (allocated(error)) return
        call check(error, ccol == 5, "cursor on the newline")
        if (allocated(error)) return
        ! A `cursor' property puts the cursor on the string.
        call string_put_text_property(0_int64, 1_int64, value_type(name="cursor"), value_type(1_int64), string, st)
        call overlay_put(ov, "after_string", string, st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, ccol == 2, "cursor on the space")
        if (allocated(error)) return
        ! Its face.
        call string_put_text_property(0_int64, 1_int64, value_type(name="face"), value_type(name="region"), string, st)
        call overlay_put(ov, "after_string", string, st)

        do r = 1, 2
            allocate (rows(r)%glyphs(8))
        end do

        call init_faces()
        call init_frame_faces(f, "dumb")
        call layout_window(bi, 1_int64, 3_int64, 8, rows, crow, ccol, end_pos, at_end, 0_int64, 0_int64, f)
        region_id = rows(1)%glyphs(3)%face_id
        call check(error, region_id /= DEFAULT_FACE_ID .and. f%faces(region_id)%tty_reverse_p, "in the region face")
        if (allocated(error)) return
        call check(error, all(rows(1)%glyphs(3:5)%face_id == region_id) .and. &
                   all(rows(1)%glyphs(1:2)%face_id == DEFAULT_FACE_ID), "the space only")
        if (allocated(error)) return
        ! An overlay for another window is not shown.
        call destroy_overlay(ov)
        call make_overlay(ov2, 2_int64, 2_int64, st)
        call overlay_put(ov2, "before_string", value_type(text="BS"), st)
        call overlay_put(ov2, "window", make_window_ref(handle_type()), st)
        call layout1(bi, 1_int64, text, ccol)
        call check(error, text == "ab", text)
        call destroy_overlay(ov2)
        call close_buffer(h, st)
    end subroutine test_overlay_string_properties

    !> Invisible text whose spec asks for an ellipsis: the cursor of a
    !> point in it is on the first period.
    subroutine test_invisible_ellipsis(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-ellipsis*", "abcdef", h, bi)
        call add_to_invisibility_spec("foo", ellipsis=.true.)
        call put_text_property(2_int64, 5_int64, value_type(name="invisible"), value_type(name="foo"), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "a...ef", text)
        if (allocated(error)) return
        call check(error, ccol == 1, "cursor on the ellipsis")
        if (allocated(error)) return
        call layout1(bi, 2_int64, text, ccol)
        call check(error, ccol == 1, "at its start")
        if (allocated(error)) return
        call layout1(bi, 5_int64, text, ccol)
        call check(error, ccol == 4, "after it")
        call close_buffer(h, st)
    end subroutine test_invisible_ellipsis

    !> Invisible text without an ellipsis, and a before-string at a
    !> position in it.
    subroutine test_invisible(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, ov
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-invisible*", "abcdef", h, bi)
        call put_text_property(2_int64, 5_int64, value_type(name="invisible"), value_type(.true.), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "aef", text)
        if (allocated(error)) return
        call check(error, ccol == 1, "cursor on e")
        if (allocated(error)) return
        call erase_buffer(st)
        call insert("abcdef", st)
        call put_text_property(3_int64, 5_int64, value_type(name="invisible"), value_type(.true.), st)
        call make_overlay(ov, 3_int64, 3_int64, st)
        call overlay_put(ov, "before_string", value_type(text="BS"), st)
        call layout1(bi, 4_int64, text, ccol)
        call check(error, text == "abBSef", text)
        if (allocated(error)) return
        call check(error, ccol == 4, "cursor after the before-string")
        call destroy_overlay(ov)
        call close_buffer(h, st)
    end subroutine test_invisible

    !> An invisible newline joins two lines on one row.
    subroutine test_invisible_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-newline*", "ab"//achar(10)//"cd", h, bi)
        call put_text_property(3_int64, 4_int64, value_type(name="invisible"), value_type(.true.), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "abcd", text)
        if (allocated(error)) return
        call check(error, ccol == 2, "cursor on c")
        if (allocated(error)) return
        call layout1(bi, 4_int64, text, ccol)
        call check(error, ccol == 2, "point on c")
        call close_buffer(h, st)
    end subroutine test_invisible_newline

    !> A display string replaces its text: the cursor of a point at its
    !> start is on the string, of a point inside after it; an empty
    !> string hides the text.
    subroutine test_display_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-display*", "abcdef", h, bi)
        call put_text_property(3_int64, 5_int64, value_type(name="display"), value_type(text="XY"), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "abXYef", text)
        if (allocated(error)) return
        call check(error, ccol == 2, "at its start")
        if (allocated(error)) return
        call layout1(bi, 4_int64, text, ccol)
        call check(error, ccol == 4, "inside")
        if (allocated(error)) return
        call layout1(bi, 5_int64, text, ccol)
        call check(error, ccol == 4, "after")
        if (allocated(error)) return
        call put_text_property(3_int64, 5_int64, value_type(name="display"), value_type(text=""), st)
        call layout1(bi, 3_int64, text, ccol)
        call check(error, text == "abef", text)
        if (allocated(error)) return
        call check(error, ccol == 2, "empty string")
        call close_buffer(h, st)
    end subroutine test_display_string

    !> A `(space :align-to N)' display spec.
    subroutine test_display_space(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi
        character(len=:), allocatable :: text
        integer :: ccol

        call buffer_with("*xdisp-space*", "ab"//achar(9)//"cd", h, bi)
        call put_text_property(3_int64, 4_int64, value_type(name="display"), &
                               list(value_type(name="space"), value_type(name=":align-to"), value_type(10_int64)), st)
        call layout1(bi, 4_int64, text, ccol)
        call check(error, text == "ab        cd", text)
        if (allocated(error)) return
        call check(error, ccol == 10, "cursor on c")
        call close_buffer(h, st)
    end subroutine test_display_space

    !> The region (here 2 to 6) is drawn in the region face, and a newline
    !> inside it extends the face to the end of the row.
    subroutine test_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type) :: rows(2)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol, r
        integer(int32) :: region_id
        logical :: at_end
        type(status_type) :: st
        type(frame_type) :: f

        call buffer_with("*xdisp-region*", "abc"//achar(10)//"def", h, bi)

        do r = 1, 2
            allocate (rows(r)%glyphs(8))
        end do

        ! A monochrome terminal: the region is in inverse video.
        call init_faces()
        call init_frame_faces(f, "dumb")
        call layout_window(bi, 1_int64, 1_int64, 8, rows, crow, ccol, end_pos, at_end, 2_int64, 6_int64, f)
        region_id = rows(1)%glyphs(2)%face_id
        call check(error, region_id /= DEFAULT_FACE_ID, "the region face")
        if (allocated(error)) return
        call check(error, f%faces(region_id)%tty_reverse_p, "in inverse video")
        if (allocated(error)) return
        ! (On a monochrome terminal, the region doesn't extend.)
        call check(error, rows(1)%glyphs(1)%face_id == DEFAULT_FACE_ID .and. &
                   all(rows(1)%glyphs(2:4)%face_id == region_id) .and. &
                   all(rows(1)%glyphs(5:8)%face_id == DEFAULT_FACE_ID) .and. rows(1)%used == 4, &
                   "bc and the newline's space")
        if (allocated(error)) return
        call check(error, rows(2)%glyphs(1)%face_id == region_id .and. &
                   all(rows(2)%glyphs(2:3)%face_id == DEFAULT_FACE_ID), "d only")
        if (allocated(error)) return
        ! An 8-color terminal: white on blue, extended to the end of the row.
        f%tty%tn_max_colors = 8
        call tty_color_clear()
        call tty_register_colors(8, .false., st)
        call init_frame_faces(f, "dumb")
        call layout_window(bi, 1_int64, 1_int64, 8, rows, crow, ccol, end_pos, at_end, 2_int64, 6_int64, f)
        region_id = rows(1)%glyphs(2)%face_id
        call check(error, f%faces(region_id)%background == 4 .and. f%faces(region_id)%foreground == 7, &
                   "white on blue")
        if (allocated(error)) return
        call check(error, all(rows(1)%glyphs(2:8)%face_id == region_id) .and. rows(1)%used == 8, &
                   "region has :extend t")
        call tty_color_clear()
        f%tty%tn_max_colors = 0
        call init_frame_faces(f, "dumb")
        call close_buffer(h, st)
    end subroutine test_region

    subroutine test_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-lines*", "ab"//achar(10)//"cd", h, bi)
        call layout(bi, 1_int64, 4_int64, 10, 3, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)) == "ab" .and. row_text(rows(2)) == "cd" .and. &
                   row_text(rows(3)) == "", "rows")
        if (allocated(error)) return
        call check(error, crow == 1 .and. ccol == 0, "cursor on c")
        if (allocated(error)) return
        call check(error, at_end .and. end_pos == 6, "end")
        call close_buffer(h, st)
    end subroutine test_lines

    subroutine test_special_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        ! a TAB b, C-a, DEL, then a wide character and x.
        call buffer_with("*xdisp-chars*", "a"//achar(9)//"b"//achar(10)//achar(1)//achar(127)// &
                         achar(10)//char(230)//char(151)//char(165)//"x", h, bi)
        call layout(bi, 1_int64, 9_int64, 20, 3, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)), "a       b")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "^A^?")
        if (allocated(error)) return
        ! The wide character takes two cells; x is in column 2.
        call check(error, rows(3)%used == 3 .and. rows(3)%glyphs(2)%padding .and. &
                   rows(3)%glyphs(3)%ch == iachar("x"), "wide")
        if (allocated(error)) return
        call check(error, crow == 2 .and. ccol == 2, "cursor on x")
        if (allocated(error)) return
        call check(error, glyph_width(26085_int32) == 2 .and. glyph_width(97_int32) == 1, "widths")
        call close_buffer(h, st)
    end subroutine test_special_chars

    subroutine test_continuation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-cont*", "abcdefgh", h, bi)
        call layout(bi, 1_int64, 7_int64, 6, 3, rows, crow, ccol, end_pos, at_end)
        ! Five characters and \ in the last column, then the rest.
        call check(error, row_text(rows(1)), "abcde\")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "fgh")
        if (allocated(error)) return
        call check(error, crow == 1 .and. ccol == 1, "cursor on g")
        call close_buffer(h, st)
    end subroutine test_continuation

    !> After a final newline there is one more (empty) line for the cursor.
    subroutine test_end_of_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-eob*", "ab"//achar(10), h, bi)
        call layout(bi, 1_int64, 4_int64, 10, 4, rows, crow, ccol, end_pos, at_end)
        call check(error, crow == 1 .and. ccol == 0 .and. at_end, "cursor after the newline")
        if (allocated(error)) return
        ! One row shows "ab" and its newline, which reaches the end of the
        ! text (as window-end = point-max upstream: the mode line says All).
        call layout(bi, 1_int64, 1_int64, 10, 1, rows, crow, ccol, end_pos, at_end)
        call check(error, at_end .and. end_pos == 4, "first line only")
        if (allocated(error)) return
        ! Text that does not fit.
        call buffer_with("*xdisp-eob*", "ab"//achar(10)//"cd", h, bi)
        call layout(bi, 1_int64, 1_int64, 10, 1, rows, crow, ccol, end_pos, at_end)
        call check(error,.not. at_end .and. end_pos == 4, "more text")
        call close_buffer(h, st)
    end subroutine test_end_of_buffer

    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: s
        type(glyph_type) :: row(60)

        ! bindings.el's standard-mode-line-format on a text terminal.
        s = mode_line_text("*scratch*", .false., .false., "All", "1", 60)
        call mode_line_row(s, 60, row)
        call check(error, glyphs_text(row), "-UUU:--- F1  *scratch*      All L1     (Fundamental) -------")
        if (allocated(error)) return
        call check(error, all(row%face_id == 1), "mode-line face")
        if (allocated(error)) return
        ! Modified; a line number of 4 digits fills (6 " L%l") up.
        s = mode_line_text("notes", .true., .false., "Top", "1234", 40)
        call mode_line_row(s, 40, row(1:40))
        call check(error, glyphs_text(row(1:40)), "-UUU:**- F1  notes          Top L1234  (")
        if (allocated(error)) return
        ! Read-only: %* shows % and %+ shows * once modified; (-3 "%p")
        ! cuts Bottom to Bot.
        s = mode_line_text("x", .true., .true., "Bottom", "3", 32)
        call mode_line_row(s, 32, row(1:32))
        call check(error, glyphs_text(row(1:32)), "-UUU:%*- F1  x              Bot ")
        if (allocated(error)) return
        s = mode_line_text("x", .false., .true., "All", "3", 0)
        call check(error, s(6:7) == "%%", "read-only")
        if (allocated(error)) return
        ! %12b pads by characters: a 3-character name of 9 bytes.
        s = mode_line_text(repeat(char(206)//char(187), 3), .false., .false., "All", "1", 40)
        call mode_line_row(s, 40, row(1:40))
        call check(error, row(14)%ch == 955 .and. row(16)%ch == 955 .and. row(17)%ch == 32 .and. &
                   row(25)%ch == 32 .and. row(28)%ch == 32 .and. row(29)%ch == iachar("A"), "padded name")
        if (allocated(error)) return
        ! A wide character cut by the right edge shows as -.
        call mode_line_row("abc"//char(230)//char(188)//char(162), 4, row(1:4))
        call check(error, glyphs_text(row(1:4)), "abc-")
    end subroutine test_mode_line

    !> Glyphs as text (ASCII; others as ?).
    function glyphs_text(g) result(s)
        type(glyph_type), intent(in) :: g(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(g)
            if (g(k)%ch < 128) then
                s = s//achar(g(k)%ch)
            else
                s = s//"?"
            end if
        end do
    end function glyphs_text

    !> %p rounds up (percent99) and shows Top, Bottom or All at the ends;
    !> %n shows Narrow in the modes.
    subroutine test_mode_line_position(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi

        call buffer_with("*xdisp-pct*", repeat("x", 199)//achar(10), h, bi)
        call check(error, mode_line_percent(bi, 2_int64, .false.), " 1%")
        if (allocated(error)) return
        call check(error, mode_line_percent(bi, 200_int64, .false.), "99%")
        if (allocated(error)) return
        call check(error, mode_line_percent(bi, 101_int64, .false.), "50%")
        if (allocated(error)) return
        call check(error, mode_line_percent(bi, 1_int64, .false.), "Top")
        if (allocated(error)) return
        call check(error, mode_line_percent(bi, 5_int64, .true.), "Bottom")
        if (allocated(error)) return
        call check(error, mode_line_percent(bi, 1_int64, .true.), "All")
        if (allocated(error)) return
        call narrow_to_region(3_int64, 10_int64, st)
        call check(error, mode_line_modes(bi), "Fundamental Narrow")
        call widen()
        call close_buffer(h, st)
    end subroutine test_mode_line_position

    !> A wide character that does not fit before the continuation column
    !> moves to the next row, the rest of the row filled with \; the next
    !> row's TABs count from the columns actually used.
    subroutine test_wide_edge(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-wide*", "abcd"//char(230)//char(188)//char(162)//achar(9)//"x", h, bi)
        call layout(bi, 1_int64, 5_int64, 6, 2, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)), "abcd\\")
        if (allocated(error)) return
        ! The wide character at column 4 of the line, the TAB to 8.
        call check(error, rows(2)%used == 5 .and. rows(2)%glyphs(1)%ch == 28450 .and. &
                   rows(2)%glyphs(5)%ch == iachar("x"), "second row")
        if (allocated(error)) return
        call check(error, crow == 1 .and. ccol == 0, "cursor on the wide character")
        call close_buffer(h, st)
    end subroutine test_wide_edge

    !> ^A and a TAB that do not fit are split: the cells that fit stay,
    !> the rest begins the next row; the cursor is on the first part.
    subroutine test_split_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-split-chars*", "abcd"//achar(1)//"z", h, bi)
        call layout(bi, 1_int64, 5_int64, 6, 2, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)), "abcd^\")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "Az")
        if (allocated(error)) return
        call check(error, crow == 0 .and. ccol == 4, "cursor on ^")
        if (allocated(error)) return
        call buffer_with("*xdisp-split-chars*", "abc"//achar(9)//"z", h, bi)
        call layout(bi, 1_int64, 4_int64, 6, 2, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)), "abc  \")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "   z")
        if (allocated(error)) return
        call check(error, crow == 0 .and. ccol == 3, "cursor on the TAB")
        call close_buffer(h, st)
    end subroutine test_split_chars

    !> Recentering moves back by screen lines, continuation rows counted.
    subroutine test_recenter_start(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi

        ! "a" x 12 shows on three rows 6 columns wide (5 + 5 + 2).
        call buffer_with("*xdisp-recenter*", repeat("a", 12)//achar(10)//"b"//achar(10)//"c", h, bi)
        call check(error, recenter_start(bi, 16_int64, 5, 3) == 6_int64, "second row of the long line")
        if (allocated(error)) return
        call check(error, recenter_start(bi, 16_int64, 5, 0) == 16_int64, "point's line")
        if (allocated(error)) return
        call check(error, recenter_start(bi, 16_int64, 5, 9) == 1_int64, "not before the start")
        call close_buffer(h, st)
    end subroutine test_recenter_start

    !> Point far below the window start: the window recenters on it.
    subroutine test_redisplay(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text, out
        character(len=12) :: line
        integer(int64) :: bi
        integer :: k

        text = ""

        do k = 1, 100
            write (line, '("line ",i0)') k
            text = text//trim(line)//achar(10)
        end do

        call buffer_with("*xdisp-big*", text, h, bi)
        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        ! The selected window shows the buffer.
        call init_frame_windows(f)
        call set_window_buffer(f%selected_window, h, st)
        ! Point at the start of line 60.
        call goto_char(line_start(60))
        call redisplay_internal(f, st)
        out = tty_take_output(f%tty)
        ! 22 text lines: point's line goes to row 22 / 2 = 11.
        call check(error, f%cursor_vpos == 11 .and. f%cursor_hpos == 0, "cursor row")
        if (allocated(error)) return
        call check(error, row_text(f%current_matrix%rows(11)), "line 60")
        if (allocated(error)) return
        call check(error, index(row_text(f%current_matrix%rows(22)), "*xdisp-big*   ") > 0 .and. &
                   index(row_text(f%current_matrix%rows(22)), "L60") > 0, "mode line")
        if (allocated(error)) return
        ! A message shows in the echo area.
        last_message = "Hello"
        call redisplay_internal(f, st)
        call check(error, row_text(f%current_matrix%rows(23)), "Hello")
        if (allocated(error)) return
        ! The mode line is the standard one (L60 padded to 6 columns).
        call check(error, index(row_text(f%current_matrix%rows(22)), "-UUU:**- F1  *xdisp-big*    ") == 1 .and. &
                   index(row_text(f%current_matrix%rows(22)), " L60    (Fundamental) ---") > 0, &
                   row_text(f%current_matrix%rows(22)))
        last_message = ""
        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
        call close_buffer(h, st)

    contains

        !> Position of the start of line `n` ("line 1" .. "line 9" take 7
        !> characters with the newline, "line 10" .. 8).
        integer(int64) function line_start(n)
            integer, intent(in) :: n

            if (n <= 10) then
                line_start = 1 + 7*(n - 1)
            else
                line_start = 1 + 7*9 + 8*(n - 10)
            end if
        end function line_start
    end subroutine test_redisplay

    !> A header line: the window's first row shows the segments at their
    !> columns, the text starts on the second row, and the window has one
    !> text line less.
    subroutine test_header_line(error)
        use buffer_mod, only: buffers
        use window_mod, only: window_body_lines
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: out, row
        integer(int64) :: bi

        call buffer_with("*xdisp-header*", "first"//achar(10)//"second"//achar(10), h, bi)
        call buffers(bi)%header_line_format%append("Name", align_to=2)
        call buffers(bi)%header_line_format%append("Size", "bold", align_to=10)
        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        call init_frame_windows(f)
        call set_window_buffer(f%selected_window, h, st)
        call goto_char(1_int64)
        call redisplay_internal(f, st)
        out = tty_take_output(f%tty)

        steps: block
            row = row_text(f%current_matrix%rows(0))
            call check(error, row(1:14), "  Name    Size")
            if (allocated(error)) exit steps
            call check(error, row_text(f%current_matrix%rows(1)), "first")
            if (allocated(error)) exit steps
            call check(error, f%cursor_vpos == 1 .and. f%cursor_hpos == 0, "cursor below the header line")
            if (allocated(error)) exit steps
            call check(error, window_body_lines(windows(f%selected_window%index)) == 21, "21 text lines")
            if (allocated(error)) exit steps
            ! No header line without a format.
            call buffers(bi)%header_line_format%clear()
            call redisplay_internal(f, st)
            call check(error, row_text(f%current_matrix%rows(0)), "first")
        end block steps

        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
        call close_buffer(h, st)
    end subroutine test_header_line

    !> The echo area shows a message as text is shown: TAB, ^A, wide
    !> characters, its first line only, continued with \.
    subroutine test_echo_area(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: out, row
        integer(int64) :: bi

        call buffer_with("*xdisp-echo*", "", h, bi)
        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        last_message = "a"//achar(9)//"b"//achar(1)//char(230)//char(188)//char(162)//"c"//achar(10)//"second"
        call redisplay_internal(f, st)
        out = tty_take_output(f%tty)
        call check(error, row_text(f%current_matrix%rows(23)), "a       b^A?c")
        if (allocated(error)) return
        last_message = repeat("m", 100)
        call redisplay_internal(f, st)
        row = row_text(f%current_matrix%rows(23))
        call check(error, row, repeat("m", 79)//"\")
        if (allocated(error)) return
        ! A wide character that does not fit: \ in both cells left.
        last_message = repeat("m", 78)//char(230)//char(188)//char(162)
        call redisplay_internal(f, st)
        row = row_text(f%current_matrix%rows(23))
        call check(error, row, repeat("m", 78)//"\\")
        last_message = ""
        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
        call close_buffer(h, st)
    end subroutine test_echo_area

    !> `message' while the minibuffer is active shows " [...]" after the
    !> minibuffer's text (set-minibuffer-message), not in the echo area;
    !> an empty message clears it.
    subroutine test_minibuffer_message(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi

        call buffer_with(" *Minibuf-1*", "", h, bi)
        call ensure_frame_windows()
        last_message = ""
        call set_window_buffer(selected_frame%minibuffer_window, h, st)
        call message("Mark set")
        call check(error, allocated(set_minibuffer_message_text), "shown")
        if (allocated(error)) return
        call check(error, set_minibuffer_message_text, " [Mark set]")
        if (allocated(error)) return
        call check(error, last_message, "")
        if (allocated(error)) return
        call message("  [Done]")
        call check(error, set_minibuffer_message_text, "  [Done]")
        if (allocated(error)) return
        call message("")
        call check(error,.not. allocated(set_minibuffer_message_text), "cleared")
        windows(selected_frame%minibuffer_window%index)%buffer = null_handle()
        call message("Mark set")
        if (.not. allocated(error)) call check(error, last_message, "Mark set")
        last_message = ""
        call close_buffer(h, st)
    end subroutine test_minibuffer_message

    !> The whole editor on a pty: type, then C-x C-c.
    subroutine test_emacs_main(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(status_type) :: st
        integer(int32) :: master, slave
        character(len=4096) :: buffer
        character(len=:), allocatable :: out
        integer(int64) :: n

        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return
        call set_tty_size(slave, 40_int32, 8_int32, st)
        ! Raw mode first, so C-c is not taken as an interrupt.
        call init_sys_modes(slave, old, st)
        call emacs_write(master, "hi"//achar(24)//achar(3), st)
        call emacs_main("xterm-256color", slave, st)
        call check(error, st%ok(), "emacs_main: "//format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "hi")
        out = ""

        do while (input_available(master, 100_int32))
            call emacs_read(master, buffer, n, st)
            if (n <= 0) exit
            out = out//buffer(1:n)
        end do

        if (.not. allocated(error)) then
            call check(error, index(out, achar(27)//"[?1049h") > 0 .and. &
                       index(out, achar(27)//"[?1049l") > 0, "enters and leaves full screen")
        end if

        call reset_sys_modes(slave, old, st)
        call close_fd(slave)
        call close_fd(master)
    end subroutine test_emacs_main

    !> A frame split in three: two windows side by side above one, each
    !> with its mode line, a vertical border between the upper two.
    subroutine test_split_redisplay(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(handle_type) :: h, below, right, top_left
        type(status_type) :: st
        character(len=:), allocatable :: text, out, row
        character(len=12) :: line
        integer(int64) :: bi
        integer :: k

        text = ""

        do k = 1, 40
            write (line, '("line ",i0)') k
            text = text//trim(line)//achar(10)
        end do

        call buffer_with("*xdisp-split*", text, h, bi)
        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        ! The selected window shows the buffer.
        call init_frame_windows(f)
        call set_window_buffer(f%selected_window, h, st)
        call goto_char(1_int64)
        call redisplay_internal(f, st)
        top_left = f%root_window
        call split_window_internal(top_left, .false., 11, f%root_window, below, st)
        call split_window_internal(top_left, .true., 40, f%root_window, right, st)
        call redisplay_internal(f, st)
        out = tty_take_output(f%tty)
        row = row_text(f%current_matrix%rows(0))
        call check(error, row(1:6) == "line 1" .and. row(40:40) == "|" .and. row(41:46) == "line 1", row)
        if (allocated(error)) return
        row = row_text(f%current_matrix%rows(10))
        call check(error, row(1:7) == "line 11" .and. row(40:40) == "|" .and. row(41:47) == "line 11", row)
        if (allocated(error)) return
        ! The upper windows' mode lines, then the lower window's text.
        row = row_text(f%current_matrix%rows(11))
        call check(error, row(1:5) == "-UUU:" .and. row(41:45) == "-UUU:" .and. &
                   index(row(1:40), "*xdisp-split*") > 0 .and. index(row(41:), "*xdisp-split*") > 0, row)
        if (allocated(error)) return
        row = row_text(f%current_matrix%rows(12))
        call check(error, row(1:6) == "line 1" .and. len_trim(row) == 6, row)
        if (allocated(error)) return
        row = row_text(f%current_matrix%rows(22))
        call check(error, row(1:5) == "-UUU:" .and. index(row, "(Fundamental)") > 0, row)
        if (allocated(error)) return
        ! The cursor is in the selected (upper left) window.
        call check(error, f%cursor_vpos == 0 .and. f%cursor_hpos == 0, "cursor")
        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
        call close_buffer(h, st)
    end subroutine test_split_redisplay
end module test_xdisp_mod
