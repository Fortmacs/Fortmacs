! test_font_lock_mod.f90 -- tests for font_core_mod, font_lock_mod and
! jit_lock_mod.
! Own tests; upstream's font-lock-tests.el is not ported yet.
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

!> Tests for Font Lock: the modes, the `font_lock_face' alias, keyword and
!> syntactic fontification, Just-in-time Lock and redisplay.
module test_font_lock_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type, symbols, xsymbol, list_elements, vtag, V_NULL, V_NAME, V_ARRAY
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, resolve_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, point_max
    use textprop_mod, only: put_text_property, get_text_property
    use bindings_mod, only: init_bindings
    use modes_mod, only: minor_modes
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, &
                             font_lock_highlight_type, FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_KEEP
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_fontify_buffer, font_lock_ensure, &
                             font_lock_add_keywords
    use jit_lock_mod, only: is_jit_lock_mode, jit_lock_fontify_now
    use dispextern_mod, only: glyph_row_type, DEFAULT_FACE_ID
    use frame_mod, only: frame_type
    use faces_mod, only: init_faces
    use xfaces_mod, only: init_frame_faces
    use xdisp_mod, only: layout_window
    implicit none(type, external)
    private

    public :: collect_font_lock_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_font_lock_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("global-font-lock-mode", test_global), &
                    new_unittest("font-lock-face-alias", test_alias), &
                    new_unittest("keywords", test_keywords), &
                    new_unittest("syntactic", test_syntactic), &
                    new_unittest("override-prepend-keep", test_overrides), &
                    new_unittest("jit-lock-after-change", test_after_change), &
                    new_unittest("redisplay-fontifies", test_redisplay) &
                    ]
    end subroutine collect_font_lock_mod

    !> A fresh buffer NAME holding TEXT, current.
    subroutine buffer_with(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call insert(text, st)
    end subroutine buffer_with

    !> The name of the face at POS ("" if none; the first of a list).
    function face_at(pos) result(name)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(pos, value_type(name="face"), v, st)

        if (vtag(v) == V_ARRAY) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (.not. allocated(v%a)) return
        if ((vtag(v) == V_NAME .or. vtag(v) == V_NULL)) name = symbols(xsymbol(v))%name
    end function face_at

    !> Keywords highlighting "foo" in font_lock_keyword_face.
    subroutine set_foo_keywords(keywords_only)
        logical, intent(in) :: keywords_only
        type(font_lock_keyword_type) :: k(1)

        k(1) = font_lock_keyword("foo")
        call font_lock_defaults%set_local(k, keywords_only=keywords_only)
    end subroutine set_foo_keywords

    !> Global Font Lock mode turns Font Lock on in buffers whose name
    !> doesn't start with a space, and off again.
    subroutine test_global(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, hidden
        type(status_type) :: st

        call init_bindings()
        call buffer_with(" *hidden-fl*", "x", hidden)
        call buffer_with("*fl-global*", "x", h)
        call minor_modes%global_font_lock_mode%enable(st)
        call check(error, st%ok() .and. minor_modes%font_lock_mode%enabled(), "on in a buffer")
        if (allocated(error)) return
        call set_buffer(hidden, st)
        call check(error,.not. minor_modes%font_lock_mode%enabled(), "not in a hidden buffer")
        if (allocated(error)) return
        call set_buffer(h, st)
        call minor_modes%global_font_lock_mode%disable(st)
        call check(error, st%ok() .and. .not. minor_modes%font_lock_mode%enabled(), "off again")
        call close_buffer(h, st)
        call close_buffer(hidden, st)
    end subroutine test_global

    !> With Font Lock on, `font_lock_face' gives text its face.
    subroutine test_alias(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call buffer_with("*fl-alias*", "abc", h)
        call put_text_property(1_int64, 3_int64, value_type(name="font_lock_face"), value_type(name="bold"), st)
        call check(error, face_at(1_int64) == "", "no alias without Font Lock")
        if (allocated(error)) return
        call minor_modes%font_lock_mode%enable(st)
        call check(error, face_at(1_int64) == "bold" .and. face_at(3_int64) == "", "the alias")
        if (allocated(error)) return
        call minor_modes%font_lock_mode%disable(st)
        call check(error, face_at(1_int64) == "", "no alias after")
        call close_buffer(h, st)
    end subroutine test_alias

    !> A keyword's matches get its face; Font Lock uses Just-in-time Lock.
    subroutine test_keywords(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call buffer_with("*fl-keywords*", "a foo b foo", h)
        call set_foo_keywords(.true.)
        call minor_modes%font_lock_mode%enable(st)
        call check(error, st%ok() .and. is_jit_lock_mode(), "jit-lock")
        if (allocated(error)) return
        call check(error, face_at(3_int64) == "", "not fontified yet")
        if (allocated(error)) return
        call font_lock_ensure(st)
        call check(error, face_at(3_int64) == "font_lock_keyword_face" .and. face_at(5_int64) == "font_lock_keyword_face" &
                   .and. face_at(6_int64) == "" .and. face_at(9_int64) == "font_lock_keyword_face", "both foos")
        if (allocated(error)) return
        call minor_modes%font_lock_mode%disable(st)
        call check(error, face_at(3_int64) == "" .and. .not. is_jit_lock_mode(), "unfontified when off")
        call close_buffer(h, st)
    end subroutine test_keywords

    !> Strings get font_lock_string_face from the syntax table.
    subroutine test_syntactic(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call buffer_with("*fl-syntactic*", 'x "foo" y', h)
        call set_foo_keywords(.false.)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call check(error, face_at(1_int64) == "" .and. face_at(3_int64) == "font_lock_string_face" .and. &
                   face_at(7_int64) == "font_lock_string_face" .and. face_at(8_int64) == "", &
                   "the string, quotes included; keywords don't override it")
        call close_buffer(h, st)
    end subroutine test_syntactic

    !> Keywords that prepend to or keep existing faces.
    subroutine test_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(font_lock_keyword_type) :: k(2)
        type(font_lock_highlight_type) :: hl(1)
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)

        call init_bindings()
        call buffer_with("*fl-overrides*", "foo bar", h)
        k(1) = font_lock_keyword("foo", face="bold")
        hl(1) = font_lock_highlight(0, "italic", FONT_LOCK_OVERRIDE_PREPEND)
        k(2) = font_lock_keyword("o b", hl)
        call font_lock_defaults%set_local(k, keywords_only=.true.)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call get_text_property(3_int64, value_type(name="face"), v, st)
        call check(error, vtag(v) == V_ARRAY, "a list of faces")
        if (allocated(error)) return
        call list_elements(v, elts)
        call check(error, size(elts) == 2, "two faces")
        if (allocated(error)) return
        call check(error, symbols(xsymbol(elts(1)))%name == "italic" .and. symbols(xsymbol(elts(2)))%name == "bold", &
                   "italic prepended")
        if (allocated(error)) return
        call check(error, face_at(5_int64) == "italic", "italic alone after foo")
        call close_buffer(h, st)
    end subroutine test_overrides

    !> A change marks the changed text as not fontified.
    subroutine test_after_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: v

        call init_bindings()
        call buffer_with("*fl-change*", "foo"//achar(10)//"bar"//achar(10), h)
        call set_foo_keywords(.true.)
        call minor_modes%font_lock_mode%enable(st)
        call jit_lock_fontify_now(st)
        call get_text_property(6_int64, value_type(name="fontified"), v, st)
        call check(error, allocated(v%a), "fontified")
        if (allocated(error)) return
        call goto_char(6_int64)
        call insert("foo", st)
        call get_text_property(6_int64, value_type(name="fontified"), v, st)
        call check(error,.not. allocated(v%a), "refontify the change")
        if (allocated(error)) return
        call jit_lock_fontify_now(st)
        call check(error, face_at(6_int64) == "font_lock_keyword_face", "fontified again")
        call close_buffer(h, st)
    end subroutine test_after_change

    !> Redisplay fontifies the text it shows and draws it in the faces.
    subroutine test_redisplay(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(glyph_row_type) :: rows(2)
        type(frame_type) :: f
        integer(int64) :: bi, end_pos
        integer :: crow, ccol, r
        logical :: at_end

        call init_bindings()
        call init_faces()
        call init_frame_faces(f, "dumb")
        call buffer_with("*fl-redisplay*", "a foo", h)
        call set_foo_keywords(.true.)
        call minor_modes%font_lock_mode%enable(st)
        call resolve_buffer(h, bi, st)

        do r = 1, 2
            allocate (rows(r)%glyphs(10))
        end do

        call layout_window(bi, 1_int64, point_max(), 10, rows, crow, ccol, end_pos, at_end, f=f)
        call check(error, rows(1)%glyphs(1)%face_id == DEFAULT_FACE_ID .and. rows(1)%glyphs(3)%face_id /= DEFAULT_FACE_ID &
                   .and. f%faces(rows(1)%glyphs(3)%face_id)%tty_bold_p, "foo in bold (mono keyword face)")
        call close_buffer(h, st)
    end subroutine test_redisplay
end module test_font_lock_mod
