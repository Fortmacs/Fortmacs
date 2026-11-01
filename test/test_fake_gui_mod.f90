! test_fake_gui_mod.f90 -- redisplay on a fake window-system frame:
! layout in pixels through the redisplay interface (fake_gui_mod), and
! the same text as on a terminal.
! Own tests; upstream's xdisp-tests.el needs Lisp and real GUI frames.
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

module test_fake_gui_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, resolve_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char
    use value_mod, only: list, value_type
    use textprop_mod, only: put_text_property
    use dispextern_mod, only: FILLED_BOX_CURSOR, MODE_LINE_ACTIVE_FACE_ID, MODE_LINE_INACTIVE_FACE_ID, &
                              VERTICAL_BORDER_FACE_ID
    use faces_mod, only: init_faces
    use xfaces_mod, only: init_frame_faces
    use terminfo_mod, only: terminfo_type, builtin_xterm
    use term_mod, only: init_tty_entry
    use termchar_mod, only: tty_take_output
    use frame_mod, only: frame_type
    use window_mod, only: delete_window_tree, delete_window_object, split_window_internal, set_window_buffer, windows
    use xdisp_mod, only: redisplay_internal, init_frame_windows, mode_line_text, mode_line_modes
    use keyboard_mod, only: last_message
    use fake_gui_mod, only: fake_device_type, fake_font_driver_type, make_fake_frame, FAKE_COLUMN_WIDTH, &
                            FAKE_LINE_HEIGHT
    implicit none(type, external)
    private

    public :: collect_fake_gui_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_fake_gui_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("fake-gui-same-text-as-tty", test_same_text), &
                    new_unittest("fake-gui-proportional-wrap", test_proportional), &
                    new_unittest("fake-gui-tall-face", test_tall_face), &
                    new_unittest("fake-gui-mode-line", test_mode_line), &
                    new_unittest("fake-gui-window-borders", test_borders) &
                    ]
    end subroutine collect_fake_gui_mod

    subroutine buffer_with(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine buffer_with

    !> A fake frame `cols' by `lines' showing buffer `h' in its window,
    !> its faces realized with the fake fonts.
    subroutine fake_frame(f, device, driver, cols, lines, h)
        type(frame_type), intent(inout) :: f
        type(fake_device_type), target, intent(inout) :: device
        type(fake_font_driver_type), target, intent(inout) :: driver
        integer, intent(in) :: cols, lines
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call make_fake_frame(f, device, driver, cols, lines)
        call init_faces()
        call init_frame_faces(f, "dumb")
        call init_frame_windows(f)
        call set_window_buffer(f%selected_window, h, st)
    end subroutine fake_frame

    subroutine delete_frame_windows(f)
        type(frame_type), intent(inout) :: f

        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
    end subroutine delete_frame_windows

    !> With a monospaced font, the fake frame shows the text a terminal
    !> shows, each character cell FAKE_COLUMN_WIDTH by FAKE_LINE_HEIGHT
    !> pixels: wrapping (a TAB split at the edge, a wide character moved
    !> whole), ^A and wide characters, the cursor and the echo area.
    subroutine test_same_text(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: tty, f
        type(fake_device_type), target :: device
        type(fake_font_driver_type), target :: driver
        type(terminfo_type) :: ti
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text, out
        character(len=80) :: msg
        integer :: r, k
        integer(int32) :: codes(80)

        text = "first line"//achar(10)// &
               repeat("abcdefghij", 7)//"abcdefgh"//achar(9)//"after the tab"//achar(10)// &
               "ctrl "//achar(1)//" and tab"//achar(9)//"x"//achar(10)// &
               repeat("w", 79)//char(230)//char(151)//char(165)//"wide"//achar(10)// &
               char(230)//char(151)//char(165)//char(230)//char(156)//char(172)//" two wide"//achar(10)// &
               repeat("long line ", 20)//achar(10)//"last"
        call buffer_with("*fake-gui-same*", text, h)
        call goto_char(26_int64)
        ! The terminal.
        call builtin_xterm(ti)
        call init_tty_entry(tty%tty, ti, "xterm-256color", -1_int32, st)
        call init_faces()
        call init_frame_faces(tty, "xterm-256color")
        call init_frame_windows(tty)
        call set_window_buffer(tty%selected_window, h, st)
        last_message = "Hello"
        call redisplay_internal(tty, st)
        out = tty_take_output(tty%tty)
        ! The fake frame, as large.
        call fake_frame(f, device, driver, 80, 24, h)
        call redisplay_internal(f, st)
        call check(error, st%ok() .and. device%flushes == 1 .and. device%clears == 1, "drawn and flushed")
        if (allocated(error)) return

        do r = 0, 21
            codes = device%column_text(r*FAKE_LINE_HEIGHT, 80)

            do k = 1, 80
                if (codes(k) /= tty%current_matrix%rows(r)%glyphs(k)%ch) then
                    write (msg, '("row ",i0,", column ",i0,": ",i0," /= ",i0)') r, k, codes(k), &
                        tty%current_matrix%rows(r)%glyphs(k)%ch
                    call check(error, .false., trim(msg))
                    return
                end if
            end do
        end do

        ! The wide character moved to row 5 is one glyph over two columns.
        codes = device%column_text(5*FAKE_LINE_HEIGHT, 80)
        call check(error, codes(1) == 26085 .and. codes(2) == 26085 .and. device%starts(0, 5*FAKE_LINE_HEIGHT) .and. &
                   .not. device%starts(FAKE_COLUMN_WIDTH, 5*FAKE_LINE_HEIGHT) .and. &
                   device%text_at(4*FAKE_LINE_HEIGHT, 78*FAKE_COLUMN_WIDTH) == "w\", "wide character")
        if (allocated(error)) return
        ! The echo area.
        call check(error, device%text_at(23*FAKE_LINE_HEIGHT), "Hello ")
        if (allocated(error)) return
        ! The cursor (on the e at column 14 of the second line) as a box
        ! over its cell.
        call check(error, tty%cursor_vpos == 1 .and. tty%cursor_hpos == 14, "terminal cursor")
        if (allocated(error)) return
        call check(error, device%cursor_type == FILLED_BOX_CURSOR .and. &
                   device%cursor_x == 14*FAKE_COLUMN_WIDTH .and. device%cursor_y == FAKE_LINE_HEIGHT .and. &
                   device%cursor_width == FAKE_COLUMN_WIDTH .and. device%cursor_height == FAKE_LINE_HEIGHT, &
                   "fake cursor")
        if (allocated(error)) return
        ! The TAB at column 78 is one glyph filling the row to the
        ! continuation glyph.
        call check(error, device%text_at(FAKE_LINE_HEIGHT, 77*FAKE_COLUMN_WIDTH), "h \", &
                   device%text_at(FAKE_LINE_HEIGHT, 77*FAKE_COLUMN_WIDTH))
        last_message = ""
        call delete_frame_windows(tty)
        call delete_frame_windows(f)
        call close_buffer(h, st)
    end subroutine test_same_text

    !> With a proportional font, rows wrap where the glyphs' widths fill
    !> the window: 18 i's (4 pixels each) fit the 72 pixels left of the
    !> continuation glyph of a 10-column window, 6 m's (12 each).
    subroutine test_proportional(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(fake_device_type), target :: device
        type(fake_font_driver_type), target :: driver
        type(handle_type) :: h
        type(status_type) :: st

        call buffer_with("*fake-gui-prop*", repeat("i", 19)//achar(10)//repeat("m", 7)//achar(10)//"ab", h)
        call goto_char(23_int64)
        driver%proportional = .true.
        call fake_frame(f, device, driver, 10, 8, h)
        call redisplay_internal(f, st)
        call check(error, device%text_at(0), repeat("i", 18)//"\", device%text_at(0))
        if (allocated(error)) return
        ! The continuation glyph right after the last i.
        call check(error, device%starts(72, 0) .and. device%ch(72, 0) == iachar("\"), "continuation glyph at 72")
        if (allocated(error)) return
        call check(error, device%text_at(FAKE_LINE_HEIGHT), "i ", device%text_at(FAKE_LINE_HEIGHT))
        if (allocated(error)) return
        call check(error, device%text_at(2*FAKE_LINE_HEIGHT), "mmmmmm\", device%text_at(2*FAKE_LINE_HEIGHT))
        if (allocated(error)) return
        call check(error, device%text_at(3*FAKE_LINE_HEIGHT), "m ", device%text_at(3*FAKE_LINE_HEIGHT))
        if (allocated(error)) return
        call check(error, device%text_at(4*FAKE_LINE_HEIGHT), "ab ", device%text_at(4*FAKE_LINE_HEIGHT))
        if (allocated(error)) return
        ! Point on the third m: x 24, its width 12.
        call check(error, device%cursor_x == 24 .and. device%cursor_y == 2*FAKE_LINE_HEIGHT .and. &
                   device%cursor_width == 12 .and. device%cursor_height == FAKE_LINE_HEIGHT, "cursor")
        if (allocated(error)) return
        call check(error, windows(f%selected_window%index)%cursor_vpos == 2 .and. &
                   windows(f%selected_window%index)%cursor_hpos == 2, "window cursor row and glyph")
        call delete_frame_windows(f)
        call close_buffer(h, st)
    end subroutine test_proportional

    !> Text in a face with the larger font makes its row taller, the rows
    !> below lower, and the window shows fewer of them.
    subroutine test_tall_face(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(fake_device_type), target :: device
        type(fake_font_driver_type), target :: driver
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k

        text = "ab"//achar(10)//"cd"//achar(10)//"ef"

        do k = 1, 20
            text = text//achar(10)//"x"
        end do

        call buffer_with("*fake-gui-tall*", text, h)
        call put_text_property(4_int64, 6_int64, value_type(name="face"), list(value_type(name=":height"), value_type(200_int64)), &
                               st)
        call goto_char(5_int64)
        call fake_frame(f, device, driver, 20, 10, h)
        call redisplay_internal(f, st)
        call check(error, device%text_at(0), "ab ", device%text_at(0))
        if (allocated(error)) return
        ! Row 2 from y 16 to 47, its glyphs 16 pixels wide.
        call check(error, device%text_at(16), "cd ", device%text_at(16))
        if (allocated(error)) return
        call check(error, device%ch(0, 47) == iachar("c") .and. device%starts(16, 16) .and. &
                   device%ch(16, 47) == iachar("d"), "c and d in the large font")
        if (allocated(error)) return
        call check(error, device%text_at(48), "ef ", device%text_at(48))
        if (allocated(error)) return
        ! The cursor on d: its glyph's width, the row's height.
        call check(error, device%cursor_x == 16 .and. device%cursor_y == 16 .and. device%cursor_width == 16 .and. &
                   device%cursor_height == 32, "cursor on d")
        if (allocated(error)) return
        ! 9 lines less the mode line: 128 pixels of text: 16 + 32 + 16 + 4 * 16
        ! = 128: rows ab, cd, ef and four x's; then the mode line.
        call check(error, device%text_at(112), "x ", device%text_at(112))
        if (allocated(error)) return
        call check(error, index(device%text_at(128), " U:") == 1, device%text_at(128))
        if (allocated(error)) return
        call check(error, windows(f%selected_window%index)%window_end_pos == 18_int64, "window end")
        call delete_frame_windows(f)
        call close_buffer(h, st)
    end subroutine test_tall_face

    !> The mode line of a window-system frame: the standard mode line as
    !> display-graphic-p shows it, in the mode line faces, padded with
    !> spaces to the window's width.
    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(fake_device_type), target :: device
        type(fake_font_driver_type), target :: driver
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: expected, shown
        integer(int64) :: bi

        call buffer_with("*fake-gui-ml*", "text", h)
        call resolve_buffer(h, bi, st)
        call fake_frame(f, device, driver, 60, 6, h)
        call redisplay_internal(f, st)
        expected = mode_line_text("*fake-gui-ml*", .true., .false., "All", "1", 0, "Fundamental", graphic=.true.)
        call check(error, expected, " U:**-  *fake-gui-ml*   All L1     (Fundamental) ")
        if (allocated(error)) return
        expected = mode_line_text("*fake-gui-ml*", .true., .false., "All", "1", 0, mode_line_modes(bi), graphic=.true.)
        ! Rows 0 to 3 of text, the mode line at y 64.
        shown = device%text_at(4*FAKE_LINE_HEIGHT)
        call check(error, len(shown) == 60, shown)
        if (.not. allocated(error)) call check(error, shown(1:len(expected)) == expected .and. &
                                               shown(len(expected) + 1:) == repeat(" ", 60 - len(expected)), shown)
        if (allocated(error)) return
        call check(error, device%face(0, 4*FAKE_LINE_HEIGHT) == MODE_LINE_ACTIVE_FACE_ID .and. &
                   device%face(59*FAKE_COLUMN_WIDTH, 4*FAKE_LINE_HEIGHT) == MODE_LINE_ACTIVE_FACE_ID, "active face")
        if (allocated(error)) return
        ! The buffer name in mode_line_buffer_id merged in.
        call check(error, device%face(8*FAKE_COLUMN_WIDTH, 4*FAKE_LINE_HEIGHT) /= MODE_LINE_ACTIVE_FACE_ID .and. &
                   device%face(7*FAKE_COLUMN_WIDTH, 4*FAKE_LINE_HEIGHT) == MODE_LINE_ACTIVE_FACE_ID, "buffer id face")
        call delete_frame_windows(f)
        call close_buffer(h, st)
    end subroutine test_mode_line

    !> Two windows side by side: no border column (the left window shows
    !> 39 characters and the continuation glyph in its 40 columns, where a
    !> terminal shows 38), the border drawn over its last pixel column down
    !> its text area, each mode line across its window, the cursor in the
    !> selected window only.
    subroutine test_borders(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(fake_device_type), target :: device
        type(fake_font_driver_type), target :: driver
        type(handle_type) :: h, right, left
        type(status_type) :: st
        character(len=:), allocatable :: row
        integer :: x

        call buffer_with("*fake-gui-split*", repeat("0123456789", 5)//achar(10)//"two", h)
        call goto_char(1_int64)
        call fake_frame(f, device, driver, 80, 12, h)
        call redisplay_internal(f, st)
        left = f%root_window
        call split_window_internal(left, .true., 40, f%root_window, right, st)
        call redisplay_internal(f, st)
        row = device%text_at(0, 0, 320)
        call check(error, row == repeat("0123456789", 3)//"012345678\", row)
        if (allocated(error)) return
        row = device%text_at(0, 320, 640)
        call check(error, row == repeat("0123456789", 3)//"012345678\", row)
        if (allocated(error)) return
        ! The border at x 319 down the 10 text lines, not the mode line.
        call check(error, all(device%border(319, 0:10*FAKE_LINE_HEIGHT - 1)) .and. &
                   .not. device%border(319, 10*FAKE_LINE_HEIGHT) .and. &
                   device%face(319, 0) == VERTICAL_BORDER_FACE_ID, "vertical border")
        if (allocated(error)) return
        call check(error, count(device%border) == 10*FAKE_LINE_HEIGHT, "one border")
        if (allocated(error)) return
        ! The mode lines: active on the left (selected), inactive on the right.
        call check(error, index(device%text_at(10*FAKE_LINE_HEIGHT, 0, 320), " U:") == 1 .and. &
                   index(device%text_at(10*FAKE_LINE_HEIGHT, 320, 640), " U:") == 1, "two mode lines")
        if (allocated(error)) return
        x = 319
        call check(error, device%face(x, 10*FAKE_LINE_HEIGHT) == MODE_LINE_ACTIVE_FACE_ID .and. &
                   device%face(x + 1, 10*FAKE_LINE_HEIGHT) == MODE_LINE_INACTIVE_FACE_ID, "mode line faces")
        if (allocated(error)) return
        call check(error, device%cursor_x == 0 .and. device%cursor_y == 0, "cursor in the left window")
        call delete_frame_windows(f)
        call close_buffer(h, st)
    end subroutine test_borders
end module test_fake_gui_mod
