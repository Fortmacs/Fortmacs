! test_term_fm.f90 -- tests for term_fm, cm_fm, termchar_fm and
! dispextern_fm: the exact bytes sent to an xterm.  Own tests: upstream
! has no terminal-output tests.
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

module test_term_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use lisp_fm, only: value_type, list, make_fixnum
    use lread_fm, only: sym
    use fns_fm, only: value_equal
    use terminfo_fm, only: terminfo_type, builtin_xterm
    use termchar_fm, only: tty_display_info, tty_take_output, tty_flush
    use cm_fm, only: cmat
    use dispextern_fm, only: glyph_type, face_type, FACE_UNDERLINE_SINGLE
    use keymap_fm, only: lookup_key
    use lisp_fm, only: make_keymap_ref
    use keyboard_fm, only: input_decode_map
    use sysdep_fm, only: open_pty, close_fd, input_available, emacs_read, &
                         set_tty_size
    use term_fm, only: delete_tty, encode_terminal_code, init_tty, init_tty_entry, &
                       tty_clear_end_of_line, tty_clear_frame, tty_clear_to_end, tty_cursor_to, &
                       tty_delete_glyphs, tty_ins_del_lines, tty_insert_glyphs, &
                       tty_reset_terminal_modes, tty_ring_bell, tty_set_terminal_modes, &
                       tty_update_end, tty_write_glyphs
    implicit none(type, external)
    private

    public :: collect_term_fm

    character, parameter :: ESC = achar(27)

contains

    subroutine collect_term_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("init-tty", test_init), &
                    new_unittest("cursor-motion", test_cursor_motion), &
                    new_unittest("clearing", test_clearing), &
                    new_unittest("write-glyphs-faces", test_write_glyphs), &
                    new_unittest("write-last-cell", test_last_cell), &
                    new_unittest("insert-delete", test_insert_delete), &
                    new_unittest("ins-del-lines-scroll-region", test_scroll_region), &
                    new_unittest("terminal-modes", test_modes), &
                    new_unittest("function-keys", test_fkeys), &
                    new_unittest("encode-terminal-code", test_encode), &
                    new_unittest("pty-output", test_pty) &
                    ]
    end subroutine collect_term_fm


    !> An xterm (the built-in entry, so the bytes don't depend on the
    !> installed terminfo database), 80x24, cursor position unknown.
    subroutine xterm(tty)
        type(tty_display_info), intent(out) :: tty
        type(terminfo_type) :: ti
        type(status_type) :: st

        call builtin_xterm(ti)
        call init_tty_entry(tty, ti, "xterm-256color", -1_int32, st)
    end subroutine xterm


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


    subroutine test_init(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty
        type(status_type) :: st

        call xterm(tty)
        call check(error, tty%wcm%cm_rows == 24 .and. tty%wcm%cm_cols == 80, "size")
        if (allocated(error)) return
        call check(error, tty%wcm%cm_autowrap .and. tty%wcm%cm_magicwrap, "wrap")
        if (allocated(error)) return
        call check(error, tty%scroll_region_ok .and. tty%line_ins_del_ok .and. &
                   tty%char_ins_del_ok, "capabilities")
        if (allocated(error)) return
        call check(error, tty%tn_max_colors, 256)
        if (allocated(error)) return
        call check(error, tty%ts_end_standout_mode, ESC//"[27m")
        if (allocated(error)) return
        call init_tty(tty, "no-such-terminal", -1_int32, st)
        call check(error, st%detail, "Terminal type no-such-terminal is not defined")
    end subroutine test_init


    subroutine test_cursor_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty

        call xterm(tty)
        ! Unknown position: absolute motion.
        call tty_cursor_to(tty, 4, 9)
        call check(error, tty_take_output(tty), ESC//"[5;10H")
        if (allocated(error)) return
        ! Three cursor-rights (9 bytes) cost more than absolute motion
        ! (term.c leaves column-absolute motion unused).
        call tty_cursor_to(tty, 4, 12)
        call check(error, tty_take_output(tty), ESC//"[5;13H")
        if (allocated(error)) return
        ! One left: backspace.
        call tty_cursor_to(tty, 4, 11)
        call check(error, tty_take_output(tty), achar(8))
        if (allocated(error)) return
        ! Start of the next line: CR LF.
        call tty_cursor_to(tty, 5, 0)
        call check(error, tty_take_output(tty), achar(13)//achar(10))
        if (allocated(error)) return
        ! Same column, far away: row-absolute.
        call tty_cursor_to(tty, 20, 0)
        call check(error, tty_take_output(tty), ESC//"[21d")
        if (allocated(error)) return
        ! Already there: nothing.
        call tty_cursor_to(tty, 20, 0)
        call check(error, tty_take_output(tty), "")
        if (allocated(error)) return
        call check(error, tty%wcm%cm_cury == 20 .and. tty%wcm%cm_curx == 0, "position")
    end subroutine test_cursor_motion


    subroutine test_clearing(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty

        call xterm(tty)
        call tty_clear_frame(tty)
        call check(error, tty_take_output(tty), ESC//"[H"//ESC//"[2J")
        if (allocated(error)) return
        call check(error, tty%wcm%cm_cury == 0 .and. tty%wcm%cm_curx == 0, "home")
        if (allocated(error)) return
        call tty_clear_end_of_line(tty, 80)
        call check(error, tty_take_output(tty), ESC//"[K")
        if (allocated(error)) return
        call tty_clear_to_end(tty)
        call check(error, tty_take_output(tty), ESC//"[J")
        if (allocated(error)) return
        ! Without clear-to-end-of-line: spaces.
        deallocate (tty%ts_clr_line)
        call cmat(tty, 3, 75)
        call tty_clear_end_of_line(tty, 80)
        call check(error, tty_take_output(tty), "     ")
        if (allocated(error)) return
        ! xterm has magic wrap (xn): the cursor waits past the last column.
        call check(error, tty%wcm%cm_curx, 80)
    end subroutine test_clearing


    subroutine test_write_glyphs(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty
        type(face_type) :: faces(0:2)

        call xterm(tty)
        faces(1)%tty_bold_p = .true.
        faces(2)%foreground = 1
        faces(2)%underline = FACE_UNDERLINE_SINGLE
        call cmat(tty, 0, 0)
        call tty_write_glyphs(tty, [glyphs("ab", 0), glyphs("cd", 1), glyphs("e", 2)], faces)
        call check(error, tty_take_output(tty), ESC//"[?25lab"// &
                   ESC//"[1mcd"//ESC//"(B"//ESC//"[m"// &
                   ESC//"[4m"//ESC//"[31me"//ESC//"(B"//ESC//"[m"//ESC//"[39;49m")
        if (allocated(error)) return
        call check(error, tty%wcm%cm_curx, 5)
        if (allocated(error)) return
        ! The cursor is shown again at the end of the update.
        call tty_update_end(tty, .false.)
        call check(error, tty_take_output(tty), ESC//"[?12l"//ESC//"[?25h"//ESC//"[?12;25h")
    end subroutine test_write_glyphs


    !> Writing the bottom-right cell would scroll an autowrapping terminal.
    subroutine test_last_cell(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty
        type(face_type) :: faces(0:0)

        call xterm(tty)
        tty%cursor_hidden = .true.
        call cmat(tty, 23, 78)
        call tty_write_glyphs(tty, glyphs("ab", 0), faces)
        ! "b" at column 78, back, then "a" inserted before it.
        call check(error, tty_take_output(tty), "b"//achar(8)//ESC//"[1@a")
        if (allocated(error)) return
        ! A single last cell without the glyph before it is left alone.
        call cmat(tty, 23, 79)
        call tty_write_glyphs(tty, glyphs("z", 0), faces)
        call check(error, tty_take_output(tty), "")
    end subroutine test_last_cell


    subroutine test_insert_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty
        type(face_type) :: faces(0:0)

        call xterm(tty)
        call cmat(tty, 2, 2)
        call tty_insert_glyphs(tty, faces, len=3)
        call check(error, tty_take_output(tty), ESC//"[3@")
        if (allocated(error)) return
        call tty_delete_glyphs(tty, 2)
        call check(error, tty_take_output(tty), ESC//"[2P")
        if (allocated(error)) return
        call tty_ins_del_lines(tty, 2, 3)
        call check(error, tty_take_output(tty), achar(13)//ESC//"[3L")
        if (allocated(error)) return
        call tty_ins_del_lines(tty, 2, -1)
        call check(error, tty_take_output(tty), ESC//"[1M")
        if (allocated(error)) return
        ! Past the bottom: nothing.
        call tty_ins_del_lines(tty, 22, 3)
        call check(error, tty_take_output(tty), "")
    end subroutine test_insert_delete


    !> Without insert/delete-line, lines scroll within a scroll region.
    subroutine test_scroll_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty

        call xterm(tty)
        deallocate (tty%ts_ins_line, tty%ts_ins_multi_lines, tty%ts_del_line, &
                    tty%ts_del_multi_lines)
        call cmat(tty, 0, 0)
        call tty_ins_del_lines(tty, 5, -2)
        call check(error, tty_take_output(tty), ESC//"[6;24r"//ESC//"[24;1H"// &
                   achar(10)//achar(10)//ESC//"[1;24r")
        if (allocated(error)) return
        call check(error, tty%wcm%cm_cury, -1)
    end subroutine test_scroll_region


    subroutine test_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty

        call xterm(tty)
        call tty_set_terminal_modes(tty)
        call check(error, tty_take_output(tty), ESC//"[?1049h"//ESC//"[22;0;0t"// &
                   ESC//"[?12;25h"//ESC//"[?1h"//ESC//"=")
        if (allocated(error)) return
        call tty_ring_bell(tty)
        call check(error, tty_take_output(tty), achar(7))
        if (allocated(error)) return
        tty%standout_mode = .true.
        call tty_reset_terminal_modes(tty)
        call check(error, tty_take_output(tty), ESC//"[27m"//ESC//"[?1l"//ESC//">"// &
                   ESC//"[?12l"//ESC//"[?25h"//ESC//"[?1049l"//ESC//"[23;0;0t"// &
                   ESC//"[39;49m"//achar(13))
    end subroutine test_modes


    !> init_tty registers the terminal's function keys in input-decode-map.
    subroutine test_fkeys(error)
        type(error_type), allocatable, intent(out) :: error
        type(tty_display_info) :: tty
        type(value_type) :: key(3), v
        type(status_type) :: st

        call xterm(tty)
        key(1) = make_fixnum(27_int64)
        key(2) = make_fixnum(int(iachar("O"), int64))
        key(3) = make_fixnum(int(iachar("A"), int64))
        call lookup_key(make_keymap_ref(input_decode_map), key, v, st)
        call check(error, value_equal(v, list(sym("up"))), "ESC O A")
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
        type(tty_display_info) :: tty
        type(terminfo_type) :: ti
        type(status_type) :: st
        integer(int32) :: master, slave
        character(len=64) :: buf
        integer(int64) :: n

        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return
        call set_tty_size(slave, 100_int32, 30_int32, st)
        call builtin_xterm(ti)
        call init_tty_entry(tty, ti, "xterm-256color", slave, st)
        call check(error, st%ok() .and. tty%wcm%cm_cols == 100 .and. tty%wcm%cm_rows == 30, &
                   "size from the pty")

        if (.not. allocated(error)) then
            call tty_cursor_to(tty, 0, 0)
            call tty_flush(tty, st)
            call check(error, input_available(master, 2000_int32), "output")

            if (.not. allocated(error)) then
                call emacs_read(master, buf, n, st)
                call check(error, buf(1:n), ESC//"[1;1H")
            end if
        end if

        call delete_tty(tty, st)
        call close_fd(slave)
        call close_fd(master)
    end subroutine test_pty

end module test_term_fm
