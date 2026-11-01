! termchar_fm.f90 -- the terminal description and its output stream.
! Ported from GNU Emacs 31.1 src/termchar.h and src/cm.h (struct cm), with
! the output side of cm.c (cmputc) and term.c's OUTPUT macros.
!
! Copyright (C) 1985-1986, 2001-2026 Free Software Foundation, Inc.
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

!> A capability that the terminal lacks is an unallocated string (NULL
!> upstream).  Output collects in the description's buffer until
!> `tty_flush' writes it to the terminal (upstream: a stdio stream); with
!> no output descriptor (tests), `tty_take_output' returns it instead.
module termchar_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use terminfo_fm, only: terminfo_type, delete_padding
    use sysdep_fm, only: emacs_tty_type, emacs_full_write
    implicit none(type, external)
    private

    public :: tty_output, tty_put_bytes, tty_take_output, tty_flush

    !> cm.c: the cost of an impossible motion.
    integer, parameter, public :: BIG = 9999

    !> Port of cm.h:struct cm, everything needed for cursor motion.
    type, public :: cm_type
        !> Cursor position; -1 in both means unknown (forces absolute motion).
        integer :: cm_cury = -1, cm_curx = -1
        character(len=:), allocatable :: cm_up, cm_down, cm_left, cm_right, &
                                         cm_home, cm_cr, cm_ll, cm_tab, cm_backtab, cm_abs, cm_habs, &
                                         cm_vabs, cm_multiup, cm_multidown, cm_multileft, &
                                         cm_multiright
        integer :: cm_cols = 0, cm_rows = 0, cm_tabwidth = 0
        logical :: cm_autowrap = .false., cm_magicwrap = .false., &
                   cm_usetabs = .false., cm_losewrap = .false., cm_autolf = .false.
        integer :: cc_up = BIG, cc_down = BIG, cc_left = BIG, cc_right = BIG, &
                   cc_home = BIG, cc_cr = BIG, cc_ll = BIG, cc_tab = BIG, &
                   cc_backtab = BIG, cc_abs = BIG, cc_habs = BIG, cc_vabs = BIG
    end type cm_type

    !> Port of termchar.h:struct tty_display_info_type (the parts in use).
    type, public :: tty_display_info_type
        !> File descriptors; -1 when not attached to a terminal.
        integer(int32) :: input = -1, output = -1
        character(len=:), allocatable :: name, type
        type(terminfo_type) :: ti
        !> Terminal modes before init_sys_modes, restored by delete_tty.
        type(emacs_tty_type) :: old_tty
        logical :: modes_saved = .false.
        !> Pending output and its length.
        character(len=:), allocatable :: obuf
        integer(int64) :: olen = 0

        type(cm_type) :: wcm
        integer :: specified_window = 0
        logical :: meta_key = .false.
        logical :: costs_set = .false.
        logical :: insert_mode = .false., standout_mode = .false., &
                   cursor_hidden = .false.
        logical :: scroll_region_ok = .false., line_ins_del_ok = .false., &
                   char_ins_del_ok = .false., memory_below_frame = .false., &
                   must_write_spaces = .false., delete_in_insert_mode = .false.

        ! Capability strings (the TS_ fields).
        character(len=:), allocatable :: ts_ins_line, ts_ins_multi_lines, &
                                         ts_bell, ts_clr_to_bottom, ts_clr_line, ts_clr_frame, &
                                         ts_set_scroll_region, ts_set_scroll_region_1, ts_del_char, &
                                         ts_del_multi_chars, ts_del_line, ts_del_multi_lines, &
                                         ts_delete_mode, ts_end_delete_mode, ts_end_insert_mode, &
                                         ts_ins_char, ts_ins_multi_chars, ts_insert_mode, &
                                         ts_pad_inserted_char, ts_end_keypad_mode, ts_keypad_mode, &
                                         ts_pad_char, ts_repeat, ts_end_standout_mode, ts_fwd_scroll, &
                                         ts_standout_mode, ts_rev_scroll, ts_end_termcap_modes, &
                                         ts_termcap_modes, ts_visible_bell, ts_cursor_normal, &
                                         ts_cursor_visible, ts_cursor_invisible, ts_set_window, &
                                         ts_enter_underline_mode, ts_exit_underline_mode, &
                                         ts_enter_bold_mode, ts_enter_italic_mode, ts_enter_dim_mode, &
                                         ts_enter_reverse_mode, ts_enter_alt_charset_mode, &
                                         ts_exit_alt_charset_mode, ts_exit_attribute_mode, &
                                         ts_enter_strike_through_mode, ts_orig_pair, &
                                         ts_set_foreground, ts_set_background
        character(len=:), allocatable :: tf_set_underline_style, tf_set_underline_color
        integer :: tn_max_colors = 0, tn_no_color_video = 0
        !> term.c:char_ins_del_vector: the cost of inserting (k > 0) or
        !> deleting (k < 0) k characters, indexed -FrameCols..FrameCols.
        integer, allocatable :: char_ins_del_cost(:)
        logical :: tf_hazeltine = .false., tf_insmode_motion = .false., &
                   tf_standout_motion = .false., tf_underscore = .false., &
                   tf_teleray = .false., tf_rgb_separate = .false.
    end type tty_display_info_type

contains

    !> Append raw bytes to the output (fwrite to tty->output).  Not a port.
    subroutine tty_put_bytes(tty, bytes)
        type(tty_display_info_type), intent(inout) :: tty
        character(len=*), intent(in) :: bytes
        character(len=:), allocatable :: grown
        integer(int64) :: n

        n = len(bytes, kind=int64)
        if (n == 0) return
        if (.not. allocated(tty%obuf)) allocate (character(len=max(4096_int64, 2*n)) :: tty%obuf)

        if (tty%olen + n > len(tty%obuf, kind=int64)) then
            allocate (character(len=max(2*len(tty%obuf, kind=int64), tty%olen + n)) :: grown)
            grown(1:tty%olen) = tty%obuf(1:tty%olen)
            call move_alloc(grown, tty%obuf)
        end if

        tty%obuf(tty%olen + 1:tty%olen + n) = bytes
        tty%olen = tty%olen + n
    end subroutine tty_put_bytes

    !> Output capability string `cap' if the terminal has it.  The OUTPUT
    !> macros of term.c (emacs_tputs with cm.c:cmputc); padding is dropped.
    !> Port of term.c:OUTPUT and term.c:OUTPUT_IF.
    subroutine tty_output(tty, cap)
        type(tty_display_info_type), intent(inout) :: tty
        character(len=:), allocatable, intent(in) :: cap

        if (.not. allocated(cap)) return
        call tty_put_bytes(tty, delete_padding(cap))
    end subroutine tty_output

    !> The pending output, which is then discarded.  Not a port.
    function tty_take_output(tty) result(s)
        type(tty_display_info_type), intent(inout) :: tty
        character(len=:), allocatable :: s

        s = ""
        if (tty%olen > 0) s = tty%obuf(1:tty%olen)
        tty%olen = 0
    end function tty_take_output

    !> Write the pending output to the terminal (fflush of tty->output).
    !> Without an output descriptor the output stays pending.  Not a port.
    subroutine tty_flush(tty, status)
        type(tty_display_info_type), intent(inout) :: tty
        type(status_type), intent(out) :: status
        integer(int64) :: written

        if (tty%output < 0 .or. tty%olen == 0) return
        call emacs_full_write(tty%output, tty%obuf(1:tty%olen), written, status)
        tty%olen = 0
    end subroutine tty_flush
end module termchar_fm
