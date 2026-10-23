! test_xfaces_fm.f90 -- tests for xfaces_fm.
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

!> Tests for xfaces_fm: merging face references, realizing terminal
!> faces, faces of buffer text, display support for attributes.
module test_xfaces_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, resolve_buffer
    use insdel_fm, only: insert
    use lisp_fm, only: list, make_string
    use lread_fm, only: sym
    use textprop_fm, only: put_text_property
    use dispextern_fm, only: DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, MODE_LINE_INACTIVE_FACE_ID, &
                             VERTICAL_BORDER_FACE_ID, FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR
    use faces_fm, only: face_attributes_type, init_faces, FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use frame_fm, only: frame_type, selected_frame
    use terminfo_fm, only: terminfo_type, builtin_xterm
    use term_fm, only: init_tty_entry
    use termchar_fm, only: tty_display_info
    use tty_colors_fm, only: tty_color_clear, tty_register_colors
    use xfaces_fm, only: init_frame_faces, merge_face_ref, lookup_named_face, merge_faces, lookup_face, &
                         face_at_buffer_position, tty_supports_face_attributes_p, frame_background_mode
    implicit none(type, external)
    private

    public :: collect_xfaces_fm

contains

    subroutine collect_xfaces_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("basic-faces-mono", test_basic_mono), &
                    new_unittest("merge-face-ref", test_merge_face_ref), &
                    new_unittest("inherit-and-merge-faces", test_inherit), &
                    new_unittest("colors-8", test_colors), &
                    new_unittest("face-at-buffer-position", test_face_at_position), &
                    new_unittest("supports-attributes", test_supports), &
                    new_unittest("background-mode", test_background_mode) &
                    ]
    end subroutine collect_xfaces_fm

    !> A monochrome terminal's basic faces: the mode lines and the
    !> vertical border in inverse video (their colors swapped).
    subroutine test_basic_mono(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        call init_faces()
        call init_frame_faces(f, "dumb")
        call check(error,.not. f%faces(DEFAULT_FACE_ID)%tty_reverse_p .and. &
                   f%faces(DEFAULT_FACE_ID)%foreground == FACE_TTY_DEFAULT_FG_COLOR .and. &
                   f%faces(DEFAULT_FACE_ID)%background == FACE_TTY_DEFAULT_BG_COLOR, "default")
        if (allocated(error)) return
        call check(error, f%faces(MODE_LINE_ACTIVE_FACE_ID)%tty_reverse_p .and. &
                   f%faces(MODE_LINE_ACTIVE_FACE_ID)%foreground == FACE_TTY_DEFAULT_BG_COLOR .and. &
                   f%faces(MODE_LINE_ACTIVE_FACE_ID)%background == FACE_TTY_DEFAULT_FG_COLOR, "mode line")
        if (allocated(error)) return
        call check(error, f%faces(MODE_LINE_INACTIVE_FACE_ID)%tty_reverse_p, "mode line inactive")
        if (allocated(error)) return
        call check(error, f%faces(VERTICAL_BORDER_FACE_ID)%tty_reverse_p, "vertical border")
        if (allocated(error)) return
        call check(error, f%faces(DEFAULT_FACE_ID)%lface%family == "default" .and. &
                   f%faces(DEFAULT_FACE_ID)%lface%weight == "normal", "fully specified default")
    end subroutine test_basic_mono

    !> Face references: a property list, a list whose first element wins,
    !> an invalid attribute.
    subroutine test_merge_face_ref(error)
        type(error_type), allocatable, intent(out) :: error
        type(face_attributes_type) :: a
        logical :: ok
        call init_faces()
        call merge_face_ref(list(sym(":foreground"), make_string("red"), sym(":inverse_video"), sym("t")), a, ok)
        call check(error, ok .and. a%foreground == "red" .and. a%inverse_video%state /= 0, "plist")
        if (allocated(error)) return
        a = face_attributes_type()
        call merge_face_ref(list(list(sym(":weight"), sym("light")), sym("bold")), a, ok)
        call check(error, ok .and. a%weight == "light", "the first reference takes precedence")
        if (allocated(error)) return
        a = face_attributes_type()
        call merge_face_ref(list(sym(":weight"), sym("heavyish"), sym(":slant"), sym("italic")), a, ok)
        call check(error,.not. ok .and. .not. allocated(a%weight) .and. a%slant == "italic", &
                   "an invalid weight, a valid slant")
        if (allocated(error)) return
        a = face_attributes_type()
        call merge_face_ref(list(sym(":underline"), list(sym(":style"), sym("wave"))), a, ok)
        call check(error, ok .and. a%underline%state /= 0, "styled underline")
        if (allocated(error)) return
        a = face_attributes_type()
        call merge_face_ref(sym("no_such_face"), a, ok)
        call check(error,.not. ok, "an unknown face")
    end subroutine test_merge_face_ref

    !> bold_italic inherits from bold and italic; escape_glyph merged
    !> into the mode line keeps its inverse video.
    subroutine test_inherit(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        integer(int32) :: id, id2
        call init_faces()
        call init_frame_faces(f, "dumb")
        id = lookup_named_face(f, "bold_italic")
        call check(error, id >= 0, "bold_italic is a face")
        if (allocated(error)) return
        call check(error, f%faces(id)%tty_bold_p .and. f%faces(id)%tty_italic_p, "bold and italic")
        if (allocated(error)) return
        id2 = lookup_named_face(f, "bold_italic")
        call check(error, id2 == id, "the face cache")
        if (allocated(error)) return
        call check(error, lookup_named_face(f, "no_such_face") == -1, "no face")
        if (allocated(error)) return
        id = merge_faces(f, "bold", 0_int32, MODE_LINE_ACTIVE_FACE_ID)
        call check(error, f%faces(id)%tty_bold_p .and. f%faces(id)%tty_reverse_p, "bold mode line")
        if (allocated(error)) return
        call check(error, merge_faces(f, "no_such_face", 0_int32, MODE_LINE_ACTIVE_FACE_ID) == &
                   MODE_LINE_ACTIVE_FACE_ID, "merging no face")
    end subroutine test_inherit

    !> On an 8-color terminal colors map to their indices; an inverse
    !> face swaps them.
    subroutine test_colors(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(status_type) :: st
        type(face_attributes_type) :: a
        integer(int32) :: id
        logical :: ok
        call init_faces()
        call tty_color_clear()
        call tty_register_colors(8, .false., st)
        f%tty%tn_max_colors = 8
        call init_frame_faces(f, "dumb")
        ! The error face: Red1 on dark backgrounds (8 colors: red).
        id = lookup_named_face(f, "error")
        call check(error, f%faces(id)%foreground == 1 .and. f%faces(id)%tty_bold_p, "error is bold red")
        if (allocated(error)) return
        a = f%faces(DEFAULT_FACE_ID)%lface
        call merge_face_ref(list(sym(":foreground"), make_string("green"), sym(":background"), &
                                 make_string("blue"), sym(":inverse_video"), sym("t")), a, ok)
        call check(error, ok, "valid")
        if (allocated(error)) return
        id = lookup_face(f, a)
        call check(error, f%faces(id)%foreground == 4 .and. f%faces(id)%background == 2, "swapped colors")
        if (allocated(error)) return
        a%foreground = "no-such-color"
        a%inverse_video = .false.
        id = lookup_face(f, a)
        call check(error, f%faces(id)%foreground == FACE_TTY_DEFAULT_FG_COLOR, "an unknown color")
        call tty_color_clear()
    end subroutine test_colors

    !> The face of buffer text: the `face' property, the region, and with
    !> extend_only only faces that extend.
    subroutine test_face_at_position(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: bi, endpos
        integer(int32) :: id
        call init_faces()
        call init_frame_faces(f, "dumb")
        call get_buffer_create("*xfaces*", h, st)
        call set_buffer(h, st)
        call insert("abcdef", st)
        call put_text_property(2_int64, 4_int64, sym("face"), sym("bold"), st)
        call resolve_buffer(h, bi, st)
        id = face_at_buffer_position(f, bi, 1_int64, endpos, 100_int64)
        call check(error, id == DEFAULT_FACE_ID .and. endpos == 2, "plain text up to the property")
        if (allocated(error)) return
        id = face_at_buffer_position(f, bi, 2_int64, endpos, 100_int64)
        call check(error, f%faces(id)%tty_bold_p .and. endpos == 4, "bold to its end")
        if (allocated(error)) return
        id = face_at_buffer_position(f, bi, 2_int64, endpos, 100_int64, extend_only=.true.)
        call check(error, id == DEFAULT_FACE_ID, "bold doesn't extend")
        if (allocated(error)) return
        id = face_at_buffer_position(f, bi, 3_int64, endpos, 100_int64, 3_int64, 6_int64)
        call check(error, f%faces(id)%tty_bold_p .and. f%faces(id)%tty_reverse_p .and. endpos == 4, &
                   "bold in the region")
        if (allocated(error)) return
        id = face_at_buffer_position(f, bi, 4_int64, endpos, 100_int64, 3_int64, 6_int64)
        call check(error,.not. f%faces(id)%tty_bold_p .and. f%faces(id)%tty_reverse_p .and. endpos == 6, &
                   "the region to its end")
        if (allocated(error)) return
        id = face_at_buffer_position(f, bi, 1_int64, endpos, 100_int64, 3_int64, 6_int64)
        call check(error, id == DEFAULT_FACE_ID .and. endpos == 2, "before the region")
        call kill_buffer(h, st)
    end subroutine test_face_at_position

    !> display-supports-face-attributes-p on xterm and on a terminal
    !> without capabilities.
    subroutine test_supports(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: saved
        type(terminfo_type) :: ti
        type(face_attributes_type) :: a
        type(status_type) :: st
        call init_faces()
        saved = selected_frame%tty
        call builtin_xterm(ti)
        call init_tty_entry(selected_frame%tty, ti, "xterm", -1_int32, st)
        call init_frame_faces(selected_frame, "xterm")
        a%underline = .true.
        call check(error, tty_supports_face_attributes_p(a), "xterm underlines")
        if (allocated(error)) return
        a = face_attributes_type()
        a%weight = "bold"
        call check(error, tty_supports_face_attributes_p(a), "xterm bold")
        if (allocated(error)) return
        a%family = "Monospace"
        call check(error,.not. tty_supports_face_attributes_p(a), "no families")
        if (allocated(error)) return
        a = face_attributes_type()
        a%weight = "normal"
        call check(error,.not. tty_supports_face_attributes_p(a), "same as default")
        if (allocated(error)) return
        a = face_attributes_type()
        a%slant = "italic"
        if (allocated(selected_frame%tty%ts_enter_italic_mode)) deallocate (selected_frame%tty%ts_enter_italic_mode)
        call check(error,.not. tty_supports_face_attributes_p(a), "no italic capability")
        selected_frame%tty = saved
        call init_frame_faces(selected_frame, "dumb")
    end subroutine test_supports

    !> frame-set-background-mode's terminal default.
    subroutine test_background_mode(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, frame_background_mode("xterm-256color", "unspecified-bg") == FACE_BACKGROUND_LIGHT .and. &
                   frame_background_mode("linux", "unspecified-bg") == FACE_BACKGROUND_DARK .and. &
                   frame_background_mode("rxvt", "unspecified-fg") == FACE_BACKGROUND_DARK, "background modes")
    end subroutine test_background_mode
end module test_xfaces_fm
