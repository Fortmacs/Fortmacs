! test_faces_fm.f90 -- tests for faces_fm.
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

module test_faces_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use faces_fm, only: named_face_type, init_faces, set_frame_display, face_spec_recalc_all, face_count, face_at, &
                        FACE_CLASS_COLOR, FACE_CLASS_MONO, FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK, &
                        FACE_UNSPECIFIED, FACE_NIL, FACE_T, FACE_PLIST, FACE_LIST, &
                        line_number, line_number_major_tick, fill_column_indicator, show_paren_match, &
                        tty_menu_selected_face, tty_menu_disabled_face, read_multiple_choice_face, tool_bar, &
                        glyphless_char, tab_line, menu, cursor
    implicit none(type, external)
    private

    public :: collect_faces_fm

contains

    subroutine collect_faces_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("face-list-order", test_face_list_order), &
                    new_unittest("color-256-light", test_color_light), &
                    new_unittest("color-8-dark", test_color_8_dark), &
                    new_unittest("mono-fallbacks", test_mono), &
                    new_unittest("default-alternative", test_default_alternative), &
                    new_unittest("relative-height", test_relative_height) &
                    ]
    end subroutine collect_faces_fm

    !> The faces are registered in the order faces.el defines them.
    subroutine test_face_list_order(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: names(*) = [character(len=27) :: &
                                                   "default", "bold", "italic", "bold_italic", "underline", &
                                                   "fixed_pitch", "fixed_pitch_serif", "variable_pitch", &
                                                   "variable_pitch_text", "shadow", "link", "link_visited", &
                                                   "highlight", "region", "secondary_selection", &
                                                   "trailing_whitespace", "line_number", "line_number_current_line", &
                                                   "line_number_major_tick", "line_number_minor_tick", &
                                                   "fill_column_indicator", "escape_glyph", "homoglyph", &
                                                   "nobreak_space", "nobreak_hyphen", "mode_line", &
                                                   "mode_line_active", "mode_line_inactive", "mode_line_highlight", &
                                                   "mode_line_emphasis", "mode_line_buffer_id", "header_line", &
                                                   "header_line_highlight", "header_line_active", &
                                                   "header_line_inactive", "vertical_border", "window_divider", &
                                                   "window_divider_first_pixel", "window_divider_last_pixel", &
                                                   "internal_border", "child_frame_border", "minibuffer_prompt", &
                                                   "margin", "fringe", "scroll_bar", "border", "cursor", "mouse", &
                                                   "tool_bar", "tab_bar", "tab_line", "tab_line_active", &
                                                   "tab_line_inactive", "menu", "help_argument_name", &
                                                   "help_key_binding", "glyphless_char", "error", "warning", &
                                                   "success", "read_multiple_choice_face", "tty_menu_enabled_face", &
                                                   "tty_menu_disabled_face", "tty_menu_selected_face", &
                                                   "show_paren_match", "show_paren_match_expression", &
                                                   "show_paren_mismatch"]
        type(named_face_type), pointer :: face
        integer :: k
        call init_faces()
        call check(error, face_count(), size(names))
        if (allocated(error)) return
        do k = 1, size(names)
            face => face_at(k)
            call check(error, trim(face%name), trim(names(k)))
            if (allocated(error)) return
        end do
    end subroutine test_face_list_order

    !> A 256-color terminal with a light background.
    subroutine test_color_light(error)
        type(error_type), allocatable, intent(out) :: error
        call init_faces()
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, 256, "tty")
        call face_spec_recalc_all()
        ! line-number: (t :inherit (shadow default)).
        call check(error, line_number%lface%inherit%state, FACE_LIST)
        if (allocated(error)) return
        call check(error, size(line_number%lface%inherit%names), 2)
        if (allocated(error)) return
        call check(error, trim(line_number%lface%inherit%names(1)), "shadow")
        if (allocated(error)) return
        call check(error, trim(line_number%lface%inherit%names(2)), "default")
        if (allocated(error)) return
        call check(error, line_number_major_tick%lface%background, "grey85")
        if (allocated(error)) return
        call check(error, line_number_major_tick%lface%weight, "bold")
        if (allocated(error)) return
        call check(error, show_paren_match%lface%background, "turquoise")
        if (allocated(error)) return
        call check(error, tty_menu_selected_face%lface%background, "red")
        if (allocated(error)) return
        call check(error, tty_menu_selected_face%lface%inverse_video%state, FACE_UNSPECIFIED)
        if (allocated(error)) return
        call check(error, tty_menu_disabled_face%lface%foreground, "lightgray")
        if (allocated(error)) return
        call check(error, tty_menu_disabled_face%lface%background, "blue")
        if (allocated(error)) return
        call check(error, read_multiple_choice_face%lface%weight, "bold")
        if (allocated(error)) return
        call check(error, size(read_multiple_choice_face%lface%inherit%names), 2)
        if (allocated(error)) return
        call check(error, trim(read_multiple_choice_face%lface%inherit%names(1)), "help_key_binding")
        if (allocated(error)) return
        call check(error, trim(read_multiple_choice_face%lface%inherit%names(2)), "underline")
        if (allocated(error)) return
        call check(error, cursor%lface%background, "black")
        if (allocated(error)) return
        ! menu: the (type tty) alternative.
        call check(error, menu%lface%inverse_video%state, FACE_T)
        if (allocated(error)) return
        ! fill-column-indicator: everything but the inherited face reset.
        call check(error, fill_column_indicator%lface%weight, "normal")
        if (allocated(error)) return
        call check(error, fill_column_indicator%lface%box%state, FACE_NIL)
        if (allocated(error)) return
        call check(error, fill_column_indicator%lface%strike_through%state, FACE_NIL)
        if (allocated(error)) return
        call check(error, fill_column_indicator%lface%inverse_video%state, FACE_NIL)
    end subroutine test_color_light

    !> An 8-color terminal with a dark background.
    subroutine test_color_8_dark(error)
        type(error_type), allocatable, intent(out) :: error
        call init_faces()
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_DARK, 8, "tty")
        call face_spec_recalc_all()
        call check(error, show_paren_match%lface%background, "steelblue3")
        if (allocated(error)) return
        ! tty-menu-disabled-face: (min-colors 16) fails.
        call check(error, tty_menu_disabled_face%lface%foreground, "white")
        if (allocated(error)) return
        ! tab-line: (min-colors 88) fails, (class mono) too.
        call check(error, tab_line%lface%inverse_video%state, FACE_T)
        if (allocated(error)) return
        call check(error, allocated(tab_line%lface%background), .false.)
        if (allocated(error)) return
        call check(error, cursor%lface%background, "white")
    end subroutine test_color_8_dark

    !> A monochrome terminal: the fallback alternatives.
    subroutine test_mono(error)
        type(error_type), allocatable, intent(out) :: error
        call init_faces()
        call set_frame_display(FACE_CLASS_MONO, FACE_BACKGROUND_LIGHT, 2, "tty")
        call face_spec_recalc_all()
        ! line-number-major-tick: (t :inherit line-number).
        call check(error, allocated(line_number_major_tick%lface%background), .false.)
        if (allocated(error)) return
        call check(error, trim(line_number_major_tick%lface%inherit%names(1)), "line_number")
        if (allocated(error)) return
        call check(error, tty_menu_selected_face%lface%inverse_video%state, FACE_T)
        if (allocated(error)) return
        call check(error, allocated(tty_menu_selected_face%lface%background), .false.)
        if (allocated(error)) return
        call check(error, trim(tty_menu_disabled_face%lface%inherit%names(1)), "shadow")
        if (allocated(error)) return
        ! show-paren-match: (min-colors 4) fails with 2 colors.
        call check(error, allocated(show_paren_match%lface%background), .false.)
        if (allocated(error)) return
        call check(error, trim(show_paren_match%lface%inherit%names(1)), "underline")
        if (allocated(error)) return
        call check(error, tab_line%lface%background, "grey")
    end subroutine test_mono

    !> tool-bar's `default' alternative applies on a terminal, where no
    !> other alternative matches.
    subroutine test_default_alternative(error)
        type(error_type), allocatable, intent(out) :: error
        call init_faces()
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, 256, "tty")
        call face_spec_recalc_all()
        call check(error, tool_bar%lface%foreground, "black")
        if (allocated(error)) return
        call check(error, allocated(tool_bar%lface%background), .false.)
        if (allocated(error)) return
        call check(error, tool_bar%lface%box%state, FACE_PLIST)
        if (allocated(error)) return
        call check(error, tool_bar%lface%box%line_width, 1)
        if (allocated(error)) return
        call check(error, tool_bar%lface%box%style, "released-button")
        if (allocated(error)) return
        ! (type x w32 ns pgtk android) (class color).
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, 16777216, "pgtk")
        call face_spec_recalc_all()
        call check(error, tool_bar%lface%background, "grey75")
        if (allocated(error)) return
        call check(error, tool_bar%lface%foreground, "black")
        if (allocated(error)) return
        ! menu: (type x-toolkit) is not an X frame's type here, so t.
        call check(error, menu%lface%inverse_video%state, FACE_T)
        if (allocated(error)) return
        call set_frame_display(FACE_CLASS_MONO, FACE_BACKGROUND_LIGHT, 2, "x")
        call face_spec_recalc_all()
        call check(error, tool_bar%lface%background, "grey")
    end subroutine test_default_alternative

    !> Float heights are relative.
    subroutine test_relative_height(error)
        type(error_type), allocatable, intent(out) :: error
        call init_faces()
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, 256, "x")
        call face_spec_recalc_all()
        call check(error, glyphless_char%lface%height%state, FACE_PLIST)
        if (allocated(error)) return
        call check(error, abs(glyphless_char%lface%height%factor - 0.6) < 1.0e-6, .true.)
        if (allocated(error)) return
        call check(error, abs(tab_line%lface%height%factor - 0.9) < 1.0e-6, .true.)
        if (allocated(error)) return
        call set_frame_display(FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, 256, "tty")
        call face_spec_recalc_all()
        call check(error, glyphless_char%lface%height%state, FACE_UNSPECIFIED)
        if (allocated(error)) return
        call check(error, glyphless_char%lface%underline%state, FACE_T)
        if (allocated(error)) return
        call check(error, trim(glyphless_char%lface%inherit%names(1)), "escape_glyph")
    end subroutine test_relative_height
end module test_faces_fm
