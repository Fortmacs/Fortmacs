! terminal_state_mod.f90 -- the terminal description and its output stream.
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
!> `terminal_flush' writes it to the terminal (upstream: a stdio stream); with
!> no output descriptor (tests), `terminal_take_output' returns it instead.
module terminal_state_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use terminfo_mod, only: terminfo_type, delete_padding
    use system_dependent_mod, only: fortmacs_terminal_type, fortmacs_full_write
    implicit none(type, external)
    private

    public :: terminal_output, terminal_put_bytes, terminal_take_output, terminal_flush

    !> cm.c: the cost of an impossible motion.
    integer, parameter, public :: BIG = 9999

    !> Port of cm.h:struct cm, everything needed for cursor motion.
    type, public :: cursor_motion_type
        !> Cursor position; -1 in both means unknown (forces absolute motion).
        integer :: cursor_motion_cury = -1, cursor_motion_curx = -1
        character(len=:), allocatable :: cursor_motion_up, cursor_motion_down, cursor_motion_left, cursor_motion_right, &
                                         cursor_motion_home, cursor_motion_cr, cursor_motion_ll, cursor_motion_tab, &
                                         cursor_motion_backtab, cursor_motion_absolute, cursor_motion_habs, &
                                         cursor_motion_vabs, cursor_motion_multiup, cursor_motion_multidown, &
                                         cursor_motion_multileft, &
                                         cursor_motion_multiright
        integer :: cursor_motion_columns = 0, cursor_motion_rows = 0, cursor_motion_tabwidth = 0
        logical :: cursor_motion_autowrap = .false., cursor_motion_magicwrap = .false., &
                   cursor_motion_usetabs = .false., cursor_motion_losewrap = .false., cursor_motion_autolf = .false.
        integer :: cc_up = BIG, cc_down = BIG, cc_left = BIG, cc_right = BIG, &
                   cc_home = BIG, cc_cr = BIG, cc_ll = BIG, cc_tab = BIG, &
                   cc_backtab = BIG, cc_absolute = BIG, cc_habs = BIG, cc_vabs = BIG
    end type cursor_motion_type

    !> Port of termchar.h:struct terminal_display_info_type (the parts in use).
    type, public :: terminal_display_info_type
        !> File descriptors; -1 when not attached to a terminal.
        integer(int32) :: input = -1, output = -1
        character(len=:), allocatable :: name, type
        type(terminfo_type) :: ti
        !> Terminal modes before initialize_system_modes, restored by delete_terminal.
        type(fortmacs_terminal_type) :: old_terminal
        logical :: modes_saved = .false.
        !> Pending output and its length.
        character(len=:), allocatable :: output_buffer
        integer(int64) :: olen = 0

        type(cursor_motion_type) :: wcm
        integer :: specified_window = 0
        logical :: meta_key = .false.
        logical :: costs_set = .false.
        logical :: insert_mode = .false., standout_mode = .false., &
                   cursor_hidden = .false.
        logical :: scroll_region_ok = .false., line_ins_delete_ok = .false., &
                   character_ins_delete_ok = .false., memory_below_frame = .false., &
                   must_write_spaces = .false., delete_in_insert_mode = .false.

        ! Capability strings (the TS_ fields).
        character(len=:), allocatable :: ts_ins_line, ts_ins_multi_lines, &
                                         ts_bell, ts_clr_to_bottom, ts_clr_line, ts_clr_frame, &
                                         ts_set_scroll_region, ts_set_scroll_region_1, ts_delete_character, &
                                         ts_delete_multi_characters, ts_delete_line, ts_delete_multi_lines, &
                                         ts_delete_mode, ts_end_delete_mode, ts_end_insert_mode, &
                                         ts_ins_character, ts_ins_multi_characters, ts_insert_mode, &
                                         ts_pad_inserted_character, ts_end_keypad_mode, ts_keypad_mode, &
                                         ts_pad_character, ts_repeat, ts_end_standout_mode, ts_fwd_scroll, &
                                         ts_standout_mode, ts_revision_scroll, ts_end_termcap_modes, &
                                         ts_termcap_modes, ts_visible_bell, ts_cursor_normal, &
                                         ts_cursor_visible, ts_cursor_invisible, ts_set_window, &
                                         ts_enter_underline_mode, ts_exit_underline_mode, &
                                         ts_enter_bold_mode, ts_enter_italic_mode, ts_enter_dim_mode, &
                                         ts_enter_reverse_mode, ts_enter_alt_character_set_mode, &
                                         ts_exit_alt_character_set_mode, ts_exit_attribute_mode, &
                                         ts_enter_strike_through_mode, ts_original_pair, &
                                         ts_set_foreground, ts_set_background
        character(len=:), allocatable :: tf_set_underline_style, tf_set_underline_color
        integer :: tn_maximum_colors = 0, tn_no_color_video = 0
        !> term.c:char_ins_del_vector: the cost of inserting (k > 0) or
        !> deleting (k < 0) k characters, indexed -FrameCols..FrameCols.
        integer, allocatable :: character_ins_delete_cost(:)
        logical :: tf_hazeltine = .false., tf_insmode_motion = .false., &
                   tf_standout_motion = .false., tf_underscore = .false., &
                   tf_teleray = .false., tf_rgb_separate = .false.
    end type terminal_display_info_type

contains

    !> Append raw bytes to the output (fwrite to tty->output).  Not a port.
    subroutine terminal_put_bytes(terminal, bytes)
        type(terminal_display_info_type), intent(inout) :: terminal
        character(len=*), intent(in) :: bytes
        character(len=:), allocatable :: grown
        integer(int64) :: n

        n = len(bytes, kind=int64)
        if (n == 0) return
        if (.not. allocated(terminal%output_buffer)) allocate (character(len=max(4096_int64, 2*n)) :: terminal%output_buffer)

        if (terminal%olen + n > len(terminal%output_buffer, kind=int64)) then
            allocate (character(len=max(2*len(terminal%output_buffer, kind=int64), terminal%olen + n)) :: grown)
            grown(1:terminal%olen) = terminal%output_buffer(1:terminal%olen)
            call move_alloc(grown, terminal%output_buffer)
        end if

        terminal%output_buffer(terminal%olen + 1:terminal%olen + n) = bytes
        terminal%olen = terminal%olen + n
    end subroutine terminal_put_bytes

    !> Output capability string `cap' if the terminal has it.  The OUTPUT
    !> macros of term.c (emacs_tputs with cm.c:cmputc); padding is dropped.
    !> Port of term.c:OUTPUT and term.c:OUTPUT_IF.
    subroutine terminal_output(terminal, cap)
        type(terminal_display_info_type), intent(inout) :: terminal
        character(len=:), allocatable, intent(in) :: cap

        if (.not. allocated(cap)) return
        call terminal_put_bytes(terminal, delete_padding(cap))
    end subroutine terminal_output

    !> The pending output, which is then discarded.  Not a port.
    function terminal_take_output(terminal) result(s)
        type(terminal_display_info_type), intent(inout) :: terminal
        character(len=:), allocatable :: s

        s = ""
        if (terminal%olen > 0) s = terminal%output_buffer(1:terminal%olen)
        terminal%olen = 0
    end function terminal_take_output

    !> Write the pending output to the terminal (fflush of tty->output).
    !> Without an output descriptor the output stays pending.  Not a port.
    subroutine terminal_flush(terminal, status)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(status_type), intent(out) :: status
        integer(int64) :: written

        if (terminal%output < 0 .or. terminal%olen == 0) return
        call fortmacs_full_write(terminal%output, terminal%output_buffer(1:terminal%olen), written, status)
        terminal%olen = 0
    end subroutine terminal_flush
end module terminal_state_mod
