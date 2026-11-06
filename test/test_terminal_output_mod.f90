! test_terminal_output_mod.f90 -- tests for terminal_output_mod, cursor_motion_mod, terminal_state_mod and
! glyph_matrices_mod: the exact bytes sent to an xterm.
! Own tests; upstream has no tests for terminal output.
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

module test_terminal_output_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use value_mod, only: value_type, list
    use utility_functions_mod, only: value_equal
    use terminfo_mod, only: terminfo_type, builtin_xterm
    use terminal_state_mod, only: terminal_display_info_type, terminal_take_output, terminal_flush
    use cursor_motion_mod, only: cmat
    use glyph_matrices_mod, only: glyph_type, face_type, FACE_UNDERLINE_SINGLE
    use keymap_mod, only: lookup_key
    use value_mod, only: make_keymap_reference, value_type
    use keyboard_mod, only: input_decode_map
    use system_dependent_mod, only: open_pty, close_fd, input_available, fortmacs_read, &
                                    set_terminal_size
    use terminal_output_mod, only: delete_terminal, encode_terminal_code, initialize_terminal, initialize_terminal_entry, &
                                   terminal_clear_end_of_line, terminal_clear_frame, terminal_clear_to_end, terminal_cursor_to, &
                                   terminal_delete_glyphs, terminal_ins_delete_lines, terminal_insert_glyphs, &
                                   terminal_reset_terminal_modes, terminal_ring_bell, terminal_set_terminal_modes, &
                                   terminal_update_end, terminal_write_glyphs, is_terminal_display_color, &
                                   terminal_display_color_cells, terminal_type, is_controlling_terminal, terminal_no_underline
    implicit none(type, external)
    private

    public :: collect_terminal_output_mod

    character, parameter :: ESC = achar(27)

contains

    !> Collect all exported unit tests.
    subroutine collect_terminal_output_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("init-tty", test_initialize), &
                    new_unittest("cursor-motion", test_cursor_motion), &
                    new_unittest("clearing", test_clearing), &
                    new_unittest("write-glyphs-faces", test_write_glyphs), &
                    new_unittest("write-last-cell", test_last_cell), &
                    new_unittest("insert-delete", test_insert_delete), &
                    new_unittest("ins-del-lines-scroll-region", test_scroll_region), &
                    new_unittest("terminal-modes", test_modes), &
                    new_unittest("function-keys", test_fkeys), &
                    new_unittest("encode-terminal-code", test_encode), &
                    new_unittest("pty-output", test_pty), &
                    new_unittest("tty-display-functions", test_terminal_functions) &
                    ]
    end subroutine collect_terminal_output_mod

    !> An xterm (the built-in entry, so the bytes don't depend on the
    !> installed terminfo database), 80x24, cursor position unknown.
    subroutine xterm(terminal)
        type(terminal_display_info_type), intent(out) :: terminal
        type(terminfo_type) :: ti
        type(status_type) :: st

        call builtin_xterm(ti)
        call initialize_terminal_entry(terminal, ti, "xterm-256color", -1_int32, st)
    end subroutine xterm

    !> tty-display-color-p, tty-display-color-cells, tty-type,
    !> controlling-tty-p and tty-no-underline on the built-in xterm.
    subroutine test_terminal_functions(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(status_type) :: st

        call xterm(terminal)
        call check(error, is_terminal_display_color(terminal) .and. terminal_display_color_cells(terminal) == 256 .and. &
                   terminal_type(terminal) == "xterm-256color" .and. is_controlling_terminal(terminal), "an xterm")
        if (allocated(error)) return
        call check(error, allocated(terminal%ts_enter_underline_mode), "it underlines")
        if (allocated(error)) return
        call terminal_no_underline(terminal)
        call check(error,.not. allocated(terminal%ts_enter_underline_mode), "tty-no-underline")
        call delete_terminal(terminal, st)
    end subroutine test_terminal_functions

    function glyphs(text, face_id) result(g)
        character(len=*), intent(in) :: text
        integer, intent(in) :: face_id
        type(glyph_type) :: g(len(text))
        integer :: k

        do k = 1, len(text)
            g(k)%ch = iachar(text(k:k))
            g(k)%face_id = face_id
        end do
    end function glyphs

    subroutine test_initialize(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(status_type) :: st

        call xterm(terminal)
        call check(error, terminal%wcm%cursor_motion_rows == 24 .and. terminal%wcm%cursor_motion_columns == 80, "size")
        if (allocated(error)) return
        call check(error, terminal%wcm%cursor_motion_autowrap .and. terminal%wcm%cursor_motion_magicwrap, "wrap")
        if (allocated(error)) return
        call check(error, terminal%scroll_region_ok .and. terminal%line_ins_delete_ok .and. &
                   terminal%character_ins_delete_ok, "capabilities")
        if (allocated(error)) return
        call check(error, terminal%tn_maximum_colors, 256)
        if (allocated(error)) return
        call check(error, terminal%ts_end_standout_mode, ESC//"[27m")
        if (allocated(error)) return
        call initialize_terminal(terminal, "no-such-terminal", -1_int32, st)
        call check(error, st%detail, "Terminal type no-such-terminal is not defined")
    end subroutine test_initialize

    subroutine test_cursor_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal

        call xterm(terminal)
        ! Unknown position: absolute motion.
        call terminal_cursor_to(terminal, 4, 9)
        call check(error, terminal_take_output(terminal), ESC//"[5;10H")
        if (allocated(error)) return
        ! Three cursor-rights (9 bytes) cost more than absolute motion
        ! (term.c leaves column-absolute motion unused).
        call terminal_cursor_to(terminal, 4, 12)
        call check(error, terminal_take_output(terminal), ESC//"[5;13H")
        if (allocated(error)) return
        ! One left: backspace.
        call terminal_cursor_to(terminal, 4, 11)
        call check(error, terminal_take_output(terminal), achar(8))
        if (allocated(error)) return
        ! Start of the next line: CR LF.
        call terminal_cursor_to(terminal, 5, 0)
        call check(error, terminal_take_output(terminal), achar(13)//achar(10))
        if (allocated(error)) return
        ! Same column, far away: row-absolute.
        call terminal_cursor_to(terminal, 20, 0)
        call check(error, terminal_take_output(terminal), ESC//"[21d")
        if (allocated(error)) return
        ! Already there: nothing.
        call terminal_cursor_to(terminal, 20, 0)
        call check(error, terminal_take_output(terminal), "")
        if (allocated(error)) return
        call check(error, terminal%wcm%cursor_motion_cury == 20 .and. terminal%wcm%cursor_motion_curx == 0, "position")
    end subroutine test_cursor_motion

    subroutine test_clearing(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal

        call xterm(terminal)
        call terminal_clear_frame(terminal)
        call check(error, terminal_take_output(terminal), ESC//"[H"//ESC//"[2J")
        if (allocated(error)) return
        call check(error, terminal%wcm%cursor_motion_cury == 0 .and. terminal%wcm%cursor_motion_curx == 0, "home")
        if (allocated(error)) return
        call terminal_clear_end_of_line(terminal, 80)
        call check(error, terminal_take_output(terminal), ESC//"[K")
        if (allocated(error)) return
        call terminal_clear_to_end(terminal)
        call check(error, terminal_take_output(terminal), ESC//"[J")
        if (allocated(error)) return
        ! Without clear-to-end-of-line: spaces.
        deallocate (terminal%ts_clr_line)
        call cmat(terminal, 3, 75)
        call terminal_clear_end_of_line(terminal, 80)
        call check(error, terminal_take_output(terminal), "     ")
        if (allocated(error)) return
        ! xterm has magic wrap (xn): the cursor waits past the last column.
        call check(error, terminal%wcm%cursor_motion_curx, 80)
    end subroutine test_clearing

    subroutine test_write_glyphs(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(face_type) :: faces(0:2)

        call xterm(terminal)
        faces(1)%terminal_bold_p = .true.
        faces(2)%foreground = 1
        faces(2)%underline = FACE_UNDERLINE_SINGLE
        call cmat(terminal, 0, 0)
        call terminal_write_glyphs(terminal, [glyphs("ab", 0), glyphs("cd", 1), glyphs("e", 2)], faces)
        call check(error, terminal_take_output(terminal), ESC//"[?25lab"// &
                   ESC//"[1mcd"//ESC//"(B"//ESC//"[m"// &
                   ESC//"[4m"//ESC//"[31me"//ESC//"(B"//ESC//"[m"//ESC//"[39;49m")
        if (allocated(error)) return
        call check(error, terminal%wcm%cursor_motion_curx, 5)
        if (allocated(error)) return
        ! The cursor is shown again at the end of the update.
        call terminal_update_end(terminal, .false.)
        call check(error, terminal_take_output(terminal), ESC//"[?12l"//ESC//"[?25h"//ESC//"[?12;25h")
    end subroutine test_write_glyphs

    !> Writing the bottom-right cell would scroll an autowrapping terminal.
    subroutine test_last_cell(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(face_type) :: faces(0:0)

        call xterm(terminal)
        terminal%cursor_hidden = .true.
        call cmat(terminal, 23, 78)
        call terminal_write_glyphs(terminal, glyphs("ab", 0), faces)
        ! "b" at column 78, back, then "a" inserted before it.
        call check(error, terminal_take_output(terminal), "b"//achar(8)//ESC//"[1@a")
        if (allocated(error)) return
        ! A single last cell without the glyph before it is left alone.
        call cmat(terminal, 23, 79)
        call terminal_write_glyphs(terminal, glyphs("z", 0), faces)
        call check(error, terminal_take_output(terminal), "")
    end subroutine test_last_cell

    subroutine test_insert_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(face_type) :: faces(0:0)

        call xterm(terminal)
        call cmat(terminal, 2, 2)
        call terminal_insert_glyphs(terminal, faces, len=3)
        call check(error, terminal_take_output(terminal), ESC//"[3@")
        if (allocated(error)) return
        call terminal_delete_glyphs(terminal, 2)
        call check(error, terminal_take_output(terminal), ESC//"[2P")
        if (allocated(error)) return
        call terminal_ins_delete_lines(terminal, 2, 3)
        call check(error, terminal_take_output(terminal), achar(13)//ESC//"[3L")
        if (allocated(error)) return
        call terminal_ins_delete_lines(terminal, 2, -1)
        call check(error, terminal_take_output(terminal), ESC//"[1M")
        if (allocated(error)) return
        ! Past the bottom: nothing.
        call terminal_ins_delete_lines(terminal, 22, 3)
        call check(error, terminal_take_output(terminal), "")
    end subroutine test_insert_delete

    !> Without insert/delete-line, lines scroll within a scroll region.
    subroutine test_scroll_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal

        call xterm(terminal)
        deallocate (terminal%ts_ins_line, terminal%ts_ins_multi_lines, terminal%ts_delete_line, &
                    terminal%ts_delete_multi_lines)
        call cmat(terminal, 0, 0)
        call terminal_ins_delete_lines(terminal, 5, -2)
        call check(error, terminal_take_output(terminal), ESC//"[6;24r"//ESC//"[24;1H"// &
                   achar(10)//achar(10)//ESC//"[1;24r")
        if (allocated(error)) return
        call check(error, terminal%wcm%cursor_motion_cury, -1)
    end subroutine test_scroll_region

    subroutine test_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal

        call xterm(terminal)
        call terminal_set_terminal_modes(terminal)
        call check(error, terminal_take_output(terminal), ESC//"[?1049h"//ESC//"[22;0;0t"// &
                   ESC//"[?12;25h"//ESC//"[?1h"//ESC//"=")
        if (allocated(error)) return
        call terminal_ring_bell(terminal)
        call check(error, terminal_take_output(terminal), achar(7))
        if (allocated(error)) return
        terminal%standout_mode = .true.
        call terminal_reset_terminal_modes(terminal)
        call check(error, terminal_take_output(terminal), ESC//"[27m"//ESC//"[?1l"//ESC//">"// &
                   ESC//"[?12l"//ESC//"[?25h"//ESC//"[?1049l"//ESC//"[23;0;0t"// &
                   ESC//"[39;49m"//achar(13))
    end subroutine test_modes

    !> initialize_terminal registers the terminal's function keys in input-decode-map.
    subroutine test_fkeys(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(value_type) :: key(3), v
        type(status_type) :: st

        call xterm(terminal)
        key(1) = value_type(27_int64)
        key(2) = value_type(int(iachar("O"), int64))
        key(3) = value_type(int(iachar("A"), int64))
        call lookup_key(make_keymap_reference(input_decode_map), key, v, st)
        call check(error, value_equal(v, list(value_type(name="up"))), "ESC O A")
    end subroutine test_fkeys

    subroutine test_encode(error)
        type(error_type), allocatable, intent(out) :: error
        type(glyph_type) :: g(4)

        g(1)%ch = 225
        g(2)%ch = 26085
        g(3)%ch = 26085
        g(3)%padding = .true.
        g(4)%ch = int(z'3FFFC3', int32)
        call check(error, encode_terminal_code(g), char(195)//char(161)// &
                   char(230)//char(151)//char(165)//char(195))
    end subroutine test_encode

    !> Output reaches a real terminal (a pty) on flush.
    subroutine test_pty(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminal_display_info_type) :: terminal
        type(terminfo_type) :: ti
        type(status_type) :: st
        integer(int32) :: master, slave
        character(len=64) :: buffer
        integer(int64) :: n

        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return
        call set_terminal_size(slave, 100_int32, 30_int32, st)
        call builtin_xterm(ti)
        call initialize_terminal_entry(terminal, ti, "xterm-256color", slave, st)
        call check(error, st%ok() .and. terminal%wcm%cursor_motion_columns == 100 .and. terminal%wcm%cursor_motion_rows == 30, &
                   "size from the pty")

        if (.not. allocated(error)) then
            call terminal_cursor_to(terminal, 0, 0)
            call terminal_flush(terminal, st)
            call check(error, input_available(master, 2000_int32), "output")

            if (.not. allocated(error)) then
                call fortmacs_read(master, buffer, n, st)
                call check(error, buffer(1:n), ESC//"[1;1H")
            end if
        end if

        call delete_terminal(terminal, st)
        call close_fd(slave)
        call close_fd(master)
    end subroutine test_pty
end module test_terminal_output_mod
