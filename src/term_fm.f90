! term_fm.f90 -- terminal output: cursor motion, clearing, insertion and
! deletion, highlighting, and function keys.
! Ported from GNU Emacs 31.1 src/term.c.
!
! Copyright (C) 1985-1987, 1993-1995, 1998, 2000-2026 Free Software
! Foundation, Inc.
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

!> Upstream's terminal functions take a frame; there is one frame per
!> terminal here, so they take the tty_display_info and the frame's size
!> is the terminal's (FrameRows, FrameCols).  Glyph-writing functions take
!> the face table to draw with, indexed by face id from 0.
!>
!> Not ported: menus, mouse (GPM), glyph production (produce_glyphs, in
!> the redisplay step), colour-mode switching (tty_setup_colors),
!> tty-mode-set-strings, the termscript, char_ins_del cost vectors, the
!> DOS/Windows/Android branches.
module term_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use lisp_fm, only: value_type, make_fixnum, list
    use lread_fm, only: sym
    use character_fm, only: MAX_MULTIBYTE_LENGTH, char_string, char_byte8_p, &
                            char_to_byte8
    use terminfo_fm, only: terminfo_type, tgetent, tgetstr, tgetnum, tgetflag, tigetstr, &
                           tigetflag, tparam, delete_padding
    use termchar_fm, only: tty_display_info, tty_output, tty_put_bytes, tty_flush
    use cm_fm, only: cmcostinit, cmgoto, cmcheckmagic, cmat, cmplus, &
                     losecursor, wcm_init
    use dispextern_fm, only: glyph_type, face_type, FACE_TTY_DEFAULT_COLOR, &
                             FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR, &
                             FACE_NO_UNDERLINE, FACE_UNDERLINE_SINGLE, &
                             face_tty_specified_color, TTY_CAP_INVERSE, TTY_CAP_UNDERLINE, &
                             TTY_CAP_UNDERLINE_STYLED, TTY_CAP_BOLD, TTY_CAP_DIM, TTY_CAP_ITALIC, &
                             TTY_CAP_STRIKE_THROUGH
    use keymap_fm, only: make_sparse_keymap, define_key, keymap_live_p
    use keyboard_fm, only: input_decode_map
    use sysdep_fm, only: get_tty_size, tabs_safe_p, init_sys_modes, &
                         reset_sys_modes
    implicit none(type, external)
    private

    public :: init_tty, init_tty_entry, delete_tty, calculate_costs, term_get_fkeys
    public :: tty_ring_bell, tty_set_terminal_modes, tty_reset_terminal_modes, &
              tty_update_end, tty_set_terminal_window, tty_set_scroll_region, &
              tty_turn_on_insert, tty_turn_off_insert, tty_turn_on_highlight, &
              tty_turn_off_highlight, tty_hide_cursor, tty_show_cursor, &
              tty_cursor_to, tty_raw_cursor_to, tty_clear_to_end, &
              tty_clear_frame, tty_clear_end_of_line, tty_write_glyphs, &
              tty_insert_glyphs, tty_delete_glyphs, tty_ins_del_lines, &
              turn_on_face, turn_off_face, encode_terminal_code, tty_capable_p

    !> True means try to flash the frame to represent a bell.
    !>
    !> See also `ring_bell_function'.  Port of dispnew.c visible-bell (with
    !> dispnew.c inverse-video and term.c visible-cursor, declared with it).
    logical, public :: visible_bell = .false., inverse_video = .false., &
                       visible_cursor = .true.

    !> term.c:enum no_color_bit (terminfo ncv).
    integer, parameter :: NC_STANDOUT = 1, NC_UNDERLINE = 2, NC_REVERSE = 4, &
                          NC_ITALIC = 8, NC_DIM = 16, NC_BOLD = 32, &
                          NC_STRIKE_THROUGH = 64

contains

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------
    !> `dest' = termcap string capability `code', unallocated if absent.
    subroutine getcap(tty, code, dest)
        type(tty_display_info), intent(in) :: tty
        character(len=*), intent(in) :: code
        character(len=:), allocatable, intent(out) :: dest
        character(len=:), allocatable :: s
        logical :: found

        s = tgetstr(tty%ti, code, found)
        if (found) dest = s
    end subroutine getcap

    !> Like `getcap' with a terminfo name.
    subroutine getcap_ti(tty, name, dest)
        type(tty_display_info), intent(in) :: tty
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: dest
        character(len=:), allocatable :: s
        logical :: found

        s = tigetstr(tty%ti, name, found)
        if (found) dest = s
    end subroutine getcap_ti

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    !> Set up `tty' as terminal type `terminal_type' on file descriptor
    !> `fd' (-1: no terminal; the size then comes from the terminfo entry):
    !> read its capabilities, register its function keys in
    !> input-decode-map, put it in raw mode, and compute motion costs.
    !> Port of term.c:init_tty.
    subroutine init_tty(tty, terminal_type, fd, status)
        type(tty_display_info), intent(out) :: tty
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(terminfo_type) :: ti

        call tgetent(terminal_type, ti, status)
        if (.not. status%ok()) return
        call init_tty_entry(tty, ti, terminal_type, fd, status)
    end subroutine init_tty

    !> init_tty with the terminal description `ti' already loaded.
    subroutine init_tty_entry(tty, ti, terminal_type, fd, status)
        type(tty_display_info), intent(out) :: tty
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: colorterm, fg, bg
        integer(int32) :: width, height
        type(status_type) :: st
        integer :: length, stat

        tty%type = terminal_type
        tty%name = terminal_type
        tty%input = fd
        tty%output = fd
        tty%ti = ti
        call getcap(tty, "al", tty%ts_ins_line)
        call getcap(tty, "AL", tty%ts_ins_multi_lines)
        call getcap(tty, "bl", tty%ts_bell)
        call getcap(tty, "bt", tty%wcm%cm_backtab)
        call getcap(tty, "cd", tty%ts_clr_to_bottom)
        call getcap(tty, "ce", tty%ts_clr_line)
        call getcap(tty, "cl", tty%ts_clr_frame)
        ! Column-absolute motion ("ch") stays unused, as upstream.
        call getcap(tty, "cm", tty%wcm%cm_abs)
        call getcap(tty, "cr", tty%wcm%cm_cr)
        call getcap(tty, "cs", tty%ts_set_scroll_region)
        call getcap(tty, "cS", tty%ts_set_scroll_region_1)
        call getcap(tty, "cv", tty%wcm%cm_vabs)
        call getcap(tty, "dc", tty%ts_del_char)
        call getcap(tty, "DC", tty%ts_del_multi_chars)
        call getcap(tty, "dl", tty%ts_del_line)
        call getcap(tty, "DL", tty%ts_del_multi_lines)
        call getcap(tty, "dm", tty%ts_delete_mode)
        call getcap(tty, "ed", tty%ts_end_delete_mode)
        call getcap(tty, "ei", tty%ts_end_insert_mode)
        call getcap(tty, "ho", tty%wcm%cm_home)
        call getcap(tty, "ic", tty%ts_ins_char)
        call getcap(tty, "IC", tty%ts_ins_multi_chars)
        call getcap(tty, "im", tty%ts_insert_mode)
        call getcap(tty, "ip", tty%ts_pad_inserted_char)
        call getcap(tty, "ke", tty%ts_end_keypad_mode)
        call getcap(tty, "ks", tty%ts_keypad_mode)
        call getcap(tty, "ll", tty%wcm%cm_ll)
        call getcap(tty, "nd", tty%wcm%cm_right)
        call getcap(tty, "do", tty%wcm%cm_down)
        if (.not. allocated(tty%wcm%cm_down)) call getcap(tty, "nl", tty%wcm%cm_down)

        if (tgetflag(tty%ti, "bs")) then
            tty%wcm%cm_left = achar(8)
        else
            call getcap(tty, "le", tty%wcm%cm_left)
        end if

        if (.not. allocated(tty%wcm%cm_left)) call getcap(tty, "bc", tty%wcm%cm_left)
        call getcap(tty, "pc", tty%ts_pad_char)
        call getcap(tty, "rp", tty%ts_repeat)
        call getcap(tty, "se", tty%ts_end_standout_mode)
        call getcap(tty, "sf", tty%ts_fwd_scroll)
        call getcap(tty, "so", tty%ts_standout_mode)
        call getcap(tty, "sr", tty%ts_rev_scroll)
        call getcap(tty, "ta", tty%wcm%cm_tab)
        call getcap(tty, "te", tty%ts_end_termcap_modes)
        call getcap(tty, "ti", tty%ts_termcap_modes)
        call getcap(tty, "up", tty%wcm%cm_up)
        call getcap(tty, "vb", tty%ts_visible_bell)
        call getcap(tty, "ve", tty%ts_cursor_normal)
        call getcap(tty, "vs", tty%ts_cursor_visible)
        call getcap(tty, "vi", tty%ts_cursor_invisible)
        call getcap(tty, "wi", tty%ts_set_window)
        call getcap(tty, "us", tty%ts_enter_underline_mode)
        call getcap(tty, "ue", tty%ts_exit_underline_mode)
        call getcap(tty, "md", tty%ts_enter_bold_mode)
        call getcap(tty, "ZH", tty%ts_enter_italic_mode)
        call getcap(tty, "mh", tty%ts_enter_dim_mode)
        call getcap(tty, "mr", tty%ts_enter_reverse_mode)
        call getcap(tty, "as", tty%ts_enter_alt_charset_mode)
        call getcap(tty, "ae", tty%ts_exit_alt_charset_mode)
        call getcap(tty, "me", tty%ts_exit_attribute_mode)
        call getcap_ti(tty, "smxx", tty%ts_enter_strike_through_mode)
        call getcap(tty, "UP", tty%wcm%cm_multiup)
        call getcap(tty, "DO", tty%wcm%cm_multidown)
        call getcap(tty, "LE", tty%wcm%cm_multileft)
        call getcap(tty, "RI", tty%wcm%cm_multiright)
        ! Colors.
        call getcap(tty, "op", tty%ts_orig_pair)

        if (allocated(tty%ts_orig_pair)) then
            call getcap(tty, "AF", tty%ts_set_foreground)
            call getcap(tty, "AB", tty%ts_set_background)

            if (.not. allocated(tty%ts_set_foreground)) then
                call getcap(tty, "Sf", tty%ts_set_foreground)
                call getcap(tty, "Sb", tty%ts_set_background)
            end if

            tty%tn_max_colors = tgetnum(tty%ti, "Co")
            call getcap_ti(tty, "setf24", fg)
            call getcap_ti(tty, "setb24", bg)

            if (allocated(fg) .and. allocated(bg)) then
                tty%ts_set_foreground = fg
                tty%ts_set_background = bg
                tty%tn_max_colors = 16777216
            else
                call getcap_ti(tty, "setrgbf", fg)
                call getcap_ti(tty, "setrgbb", bg)
                colorterm = ""
                call get_environment_variable("COLORTERM", length=length, status=stat)

                if (stat == 0 .and. length > 0) then
                    deallocate (colorterm)
                    allocate (character(len=length) :: colorterm)
                    call get_environment_variable("COLORTERM", colorterm)
                end if

                if (allocated(fg) .and. allocated(bg)) then
                    tty%ts_set_foreground = fg
                    tty%ts_set_background = bg
                    tty%tn_max_colors = 16777216
                    tty%tf_rgb_separate = .true.
                else if (tigetflag(tty%ti, "RGB")) then
                    tty%tn_max_colors = 16777216
                else if (tigetflag(tty%ti, "Tc") .or. lower(colorterm) == "truecolor") then
                    tty%ts_set_foreground = achar(27)//"[38;2;%p1%d;%p2%d;%p3%d%;m"
                    tty%ts_set_background = achar(27)//"[48;2;%p1%d;%p2%d;%p3%d%;m"
                    tty%tn_max_colors = 16777216
                    tty%tf_rgb_separate = .true.
                end if
            end if

            tty%tn_no_color_video = tgetnum(tty%ti, "NC")
            if (tty%tn_no_color_video == -1) tty%tn_no_color_video = 0
        end if

        tty%wcm%cm_magicwrap = tgetflag(tty%ti, "xn")
        tty%wcm%cm_autowrap = tty%wcm%cm_magicwrap .or. tgetflag(tty%ti, "am")
        tty%memory_below_frame = tgetflag(tty%ti, "db")
        tty%tf_hazeltine = tgetflag(tty%ti, "hz")
        tty%must_write_spaces = tgetflag(tty%ti, "in")
        tty%meta_key = tgetflag(tty%ti, "km") .or. tgetflag(tty%ti, "MT")
        tty%tf_insmode_motion = tgetflag(tty%ti, "mi")
        tty%tf_standout_motion = tgetflag(tty%ti, "ms")
        tty%tf_underscore = tgetflag(tty%ti, "ul")
        tty%tf_teleray = tgetflag(tty%ti, "xt")
        call getcap_ti(tty, "Smulx", tty%tf_set_underline_style)

        if (.not. allocated(tty%tf_set_underline_style) .and. tgetflag(tty%ti, "Su")) then
            tty%tf_set_underline_style = achar(27)//"[4:%p1%dm"
        end if

        if (allocated(tty%tf_set_underline_style)) then
            tty%tf_set_underline_color = achar(27)// &
                                         "[58:2::%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm"
        end if

        call term_get_fkeys(tty)
        width = 0
        height = 0
        if (fd >= 0) call get_tty_size(fd, width, height, st)
        tty%wcm%cm_cols = width
        tty%wcm%cm_rows = height
        if (tty%wcm%cm_cols <= 0) tty%wcm%cm_cols = tgetnum(tty%ti, "co")
        if (tty%wcm%cm_rows <= 0) tty%wcm%cm_rows = tgetnum(tty%ti, "li")

        if (tty%wcm%cm_rows < 3 .or. tty%wcm%cm_cols < 3) then
            call set_error(status, FM_GENERIC_ERROR, "Screen size "//itoa(tty%wcm%cm_cols)// &
                           "x"//itoa(tty%wcm%cm_rows)//" is too small")
            return
        end if

        tty%wcm%cm_tabwidth = tgetnum(tty%ti, "tw")
        if (.not. allocated(tty%ts_bell)) tty%ts_bell = achar(7)

        if (.not. allocated(tty%ts_fwd_scroll) .and. allocated(tty%wcm%cm_down)) then
            tty%ts_fwd_scroll = tty%wcm%cm_down
        end if

        if (tty%wcm%cm_tabwidth < 0) tty%wcm%cm_tabwidth = 8

        ! Standout or underline that take up a cell are not usable.
        if (allocated(tty%ts_standout_mode) .and. tgetnum(tty%ti, "sg") >= 0) then
            deallocate (tty%ts_standout_mode)
            if (allocated(tty%ts_end_standout_mode)) deallocate (tty%ts_end_standout_mode)
        end if

        if (allocated(tty%ts_enter_underline_mode) .and. tgetnum(tty%ti, "ug") >= 0) then
            deallocate (tty%ts_enter_underline_mode)
            if (allocated(tty%ts_exit_underline_mode)) deallocate (tty%ts_exit_underline_mode)
        end if

        ! No standout: use underline instead.
        if (.not. allocated(tty%ts_standout_mode)) then
            if (allocated(tty%ts_enter_underline_mode)) then
                tty%ts_standout_mode = tty%ts_enter_underline_mode
            end if

            if (allocated(tty%ts_exit_underline_mode)) then
                tty%ts_end_standout_mode = tty%ts_exit_underline_mode
            else if (allocated(tty%ts_end_standout_mode)) then
                deallocate (tty%ts_end_standout_mode)
            end if
        end if

        ! No end of standout: use the end of all attributes.
        if (.not. allocated(tty%ts_end_standout_mode)) then
            if (allocated(tty%ts_exit_attribute_mode)) then
                tty%ts_end_standout_mode = tty%ts_exit_attribute_mode
            else if (allocated(tty%ts_standout_mode)) then
                deallocate (tty%ts_standout_mode)
            end if
        end if

        ! Teleray: tabs, CR and LF can't be trusted.
        if (tty%tf_teleray) then
            if (allocated(tty%wcm%cm_tab)) deallocate (tty%wcm%cm_tab)
            if (allocated(tty%ts_standout_mode)) deallocate (tty%ts_standout_mode)
            if (allocated(tty%wcm%cm_cr)) deallocate (tty%wcm%cm_cr)
            if (allocated(tty%wcm%cm_down)) deallocate (tty%wcm%cm_down)
        end if

        tty%specified_window = tty%wcm%cm_rows

        if (wcm_init(tty) == -1) then
            call set_error(status, FM_GENERIC_ERROR, 'Terminal type "'//terminal_type// &
                           '" is not powerful enough to run Emacs')
            return
        end if

        tty%delete_in_insert_mode = .false.

        if (allocated(tty%ts_delete_mode) .and. allocated(tty%ts_insert_mode)) then
            tty%delete_in_insert_mode = tty%ts_delete_mode == tty%ts_insert_mode
        end if

        tty%wcm%cm_usetabs = .false.
        if (fd >= 0) tty%wcm%cm_usetabs = tabs_safe_p(fd) .and. tty%wcm%cm_tabwidth == 8
        tty%scroll_region_ok = allocated(tty%wcm%cm_abs) .and. &
                               (allocated(tty%ts_set_window) .or. allocated(tty%ts_set_scroll_region) &
                                .or. allocated(tty%ts_set_scroll_region_1))
        tty%line_ins_del_ok = ((allocated(tty%ts_ins_line) .or. allocated(tty%ts_ins_multi_lines)) &
                               .and. (allocated(tty%ts_del_line) .or. allocated(tty%ts_del_multi_lines))) &
                              .or. (tty%scroll_region_ok .and. allocated(tty%ts_fwd_scroll) &
                                    .and. allocated(tty%ts_rev_scroll))
        tty%char_ins_del_ok = (allocated(tty%ts_ins_char) .or. allocated(tty%ts_insert_mode) &
                               .or. allocated(tty%ts_pad_inserted_char) &
                               .or. allocated(tty%ts_ins_multi_chars)) &
                              .and. (allocated(tty%ts_del_char) .or. allocated(tty%ts_del_multi_chars))

        if (fd >= 0) then
            call init_sys_modes(fd, tty%old_tty, st, meta_key=tty%meta_key)
            tty%modes_saved = st%ok()
        end if

        call calculate_costs(tty)

    contains

        function lower(s) result(t)
            character(len=*), intent(in) :: s
            character(len=len(s)) :: t
            integer :: k

            t = s

            do k = 1, len(s)
                if (s(k:k) >= "A" .and. s(k:k) <= "Z") t(k:k) = achar(iachar(s(k:k)) + 32)
            end do
        end function lower
    end subroutine init_tty_entry

    !> Restore the terminal: its modes as Emacs found them.  Port of
    !> term.c:delete_tty (with the reset_sys_modes it triggers).
    subroutine delete_tty(tty, status)
        type(tty_display_info), intent(inout) :: tty
        type(status_type), intent(out) :: status

        call tty_reset_terminal_modes(tty)
        call tty_flush(tty, status)

        if (tty%modes_saved .and. tty%input >= 0) then
            call reset_sys_modes(tty%input, tty%old_tty, status)
            tty%modes_saved = .false.
        end if
    end subroutine delete_tty

    !> Compute the cursor-motion costs; until then cursor_to does nothing.
    !> Port of the terminal part of term.c:calculate_costs (the
    !> line and character insertion cost tables come with redisplay).
    subroutine calculate_costs(tty)
        type(tty_display_info), intent(inout) :: tty

        tty%costs_set = .true.
        call calculate_ins_del_char_costs(tty)
        call cmcostinit(tty)
    end subroutine calculate_costs

    !> The output cost of capability `cap' (0 if absent).  Port of
    !> term.c:string_cost (and string_cost_one_line: padding is dropped).
    integer function string_cost(cap)
        character(len=:), allocatable, intent(in) :: cap

        string_cost = 0
        if (allocated(cap)) string_cost = len(delete_padding(cap))
    end function string_cost

    !> Fill in the cost of inserting and deleting characters.  Port of
    !> term.c:calculate_ins_del_char_costs.
    subroutine calculate_ins_del_char_costs(tty)
        type(tty_display_info), intent(inout) :: tty
        integer :: ins_startup_cost, del_startup_cost, ins_cost_per_char, &
                   del_cost_per_char, cols, i

        if (allocated(tty%ts_ins_multi_chars)) then
            ins_cost_per_char = 0
            ins_startup_cost = string_cost(tty%ts_ins_multi_chars)
        else if (allocated(tty%ts_ins_char) .or. allocated(tty%ts_pad_inserted_char) .or. &
                 (allocated(tty%ts_insert_mode) .and. allocated(tty%ts_end_insert_mode))) then
            ins_startup_cost = (30*(string_cost(tty%ts_insert_mode) + &
                                    string_cost(tty%ts_end_insert_mode)))/100
            ins_cost_per_char = string_cost(tty%ts_ins_char) + string_cost(tty%ts_pad_inserted_char)
        else
            ins_startup_cost = 9999
            ins_cost_per_char = 0
        end if

        if (allocated(tty%ts_del_multi_chars)) then
            del_cost_per_char = 0
            del_startup_cost = string_cost(tty%ts_del_multi_chars)
        else if (allocated(tty%ts_del_char)) then
            del_startup_cost = string_cost(tty%ts_delete_mode) + string_cost(tty%ts_end_delete_mode)
            if (tty%delete_in_insert_mode) del_startup_cost = del_startup_cost/2
            del_cost_per_char = string_cost(tty%ts_del_char)
        else
            del_startup_cost = 9999
            del_cost_per_char = 0
        end if

        cols = tty%wcm%cm_cols
        if (allocated(tty%char_ins_del_cost)) deallocate (tty%char_ins_del_cost)
        allocate (tty%char_ins_del_cost(-cols:cols))

        do i = -1, -cols, -1
            del_startup_cost = del_startup_cost + del_cost_per_char
            tty%char_ins_del_cost(i) = del_startup_cost
        end do

        tty%char_ins_del_cost(0) = 0

        do i = 1, cols
            ins_startup_cost = ins_startup_cost + ins_cost_per_char
            tty%char_ins_del_cost(i) = ins_startup_cost
        end do
    end subroutine calculate_ins_del_char_costs

    !> Map the escape sequences the terminal's function keys send to
    !> function-key events in input-decode-map.  Port of
    !> term.c:term_get_fkeys (keys that would clash with a non-prefix
    !> binding are skipped, as upstream ignores the error).
    subroutine term_get_fkeys(tty)
        type(tty_display_info), intent(in) :: tty
        integer, parameter :: NKEYS = 66
        character(len=2), parameter :: caps(NKEYS) = [character(len=2) :: &
                                                      "kh", "kl", "ku", "kr", "kd", "%8", "%5", "@7", "@1", "*6", "%9", "@4", &
                                                      "&8", "%0", "%7", "@0", "@2", "%1", "&4", "kE", "kA", "kL", "kI", "kD", &
                                                      "kB", "@8", "K4", "K5", "K2", "K1", "K3", "k1", "k2", "k3", "k4", "k5", &
                                                      "k6", "k7", "k8", "k9", "&0", "&9", "*0", "*1", "*4", "*7", "*8", "#1", &
                                                      "#2", "#3", "#4", "%d", "%c", "%e", "%f", "%g", "%i", "!3", "  ", "  ", &
                                                      "  ", "  ", "  ", "  ", "  ", "  "]
        character(len=11), parameter :: names(NKEYS) = [character(len=11) :: &
                                                        "home", "left", "up", "right", "down", "prior", "next", "end", &
                                                        "begin", "select", "print", "execute", "undo", "redo", "menu", &
                                                        "find", "cancel", "help", "reset", "clearline", "insertline", &
                                                        "deleteline", "insertchar", "deletechar", "backtab", "kp-enter", &
                                                        "kp-1", "kp-3", "kp-5", "kp-7", "kp-9", "f1", "f2", "f3", "f4", &
                                                        "f5", "f6", "f7", "f8", "f9", "S-cancel", "S-begin", "S-find", &
                                                        "S-execute", "S-delete", "S-end", "S-clearline", "S-help", &
                                                        "S-home", "S-insert", "S-left", "S-menu", "S-next", "S-prior", &
                                                        "S-print", "S-redo", "S-right", "S-undo", "", "", "", "", "", "", &
                                                        "", ""]
        character(len=:), allocatable :: seq, k0, k_semi
        character(len=3) :: fcap
        character(len=4) :: fkey
        logical :: found, found0, found_semi
        integer :: i

        if (.not. keymap_live_p(input_decode_map)) call make_sparse_keymap(input_decode_map)

        do i = 1, NKEYS
            if (caps(i) == "  ") cycle
            seq = tgetstr(tty%ti, caps(i), found)
            if (found) call define_fkey(seq, trim(names(i)))
        end do

        ! k0 is F0 if k; exists, else F10.
        k_semi = tgetstr(tty%ti, "k;", found_semi)
        k0 = tgetstr(tty%ti, "k0", found0)

        if (found_semi) then
            if (found0) call define_fkey(k0, "f0")
            call define_fkey(k_semi, "f10")
        else if (found0) then
            call define_fkey(k0, "f10")
        end if

        ! F11..F63.
        do i = 11, 63
            if (i <= 19) then
                fcap = "F"//achar(iachar("1") + i - 11)
            else if (i <= 45) then
                fcap = "F"//achar(iachar("A") + i - 20)
            else
                fcap = "F"//achar(iachar("a") + i - 46)
            end if

            seq = tgetstr(tty%ti, trim(fcap), found)

            if (found) then
                write (fkey, '("f",i0)') i
                call define_fkey(seq, trim(fkey))
            end if
        end do

        ! Better fits when the usual capability is missing.
        call reassign("%5", "kN", "next")
        call reassign("%8", "kP", "prior")
        call reassign("kD", "kI", "insert")
        call reassign("@7", "kH", "end")

    contains

        subroutine define_fkey(sequence, name)
            character(len=*), intent(in) :: sequence, name
            type(value_type), allocatable :: key(:)
            type(status_type) :: st
            integer :: k

            if (len(sequence) == 0) return
            allocate (key(len(sequence)))

            do k = 1, len(sequence)
                key(k) = make_fixnum(int(iachar(sequence(k:k)), int64))
            end do

            call define_key(input_decode_map, key, list(sym(name)), st)
        end subroutine define_fkey

        subroutine reassign(cap1, cap2, name)
            character(len=*), intent(in) :: cap1, cap2, name
            character(len=:), allocatable :: s
            logical :: has

            s = tgetstr(tty%ti, cap1, has)
            if (has) return
            s = tgetstr(tty%ti, cap2, has)
            if (has) call define_fkey(s, name)
        end subroutine reassign
    end subroutine term_get_fkeys

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------
    !> Port of term.c:tty_ring_bell.
    subroutine tty_ring_bell(tty)
        type(tty_display_info), intent(inout) :: tty
        type(status_type) :: status

        if (allocated(tty%ts_visible_bell) .and. visible_bell) then
            call tty_output(tty, tty%ts_visible_bell)
        else
            call tty_output(tty, tty%ts_bell)
        end if

        call tty_flush(tty, status)
    end subroutine tty_ring_bell

    !> Enter full-screen mode.  Port of term.c:tty_set_terminal_modes.
    subroutine tty_set_terminal_modes(tty)
        type(tty_display_info), intent(inout) :: tty
        type(status_type) :: status

        if (allocated(tty%ts_termcap_modes)) then
            call tty_output(tty, tty%ts_termcap_modes)
        else
            ! Push the old contents off the screen.
            call tty_put_bytes(tty, repeat(achar(10), tty%wcm%cm_rows))
        end if

        if (visible_cursor) then
            call tty_output(tty, tty%ts_cursor_visible)
        else
            call tty_output(tty, tty%ts_cursor_normal)
        end if

        call tty_output(tty, tty%ts_keypad_mode)
        call losecursor(tty)
        call tty_flush(tty, status)
    end subroutine tty_set_terminal_modes

    !> Leave full-screen mode.  Port of term.c:tty_reset_terminal_modes.
    subroutine tty_reset_terminal_modes(tty)
        type(tty_display_info), intent(inout) :: tty
        type(status_type) :: status

        call tty_turn_off_highlight(tty)
        call tty_turn_off_insert(tty)
        call tty_output(tty, tty%ts_end_keypad_mode)
        call tty_output(tty, tty%ts_cursor_normal)
        call tty_output(tty, tty%ts_end_termcap_modes)
        call tty_output(tty, tty%ts_orig_pair)
        call tty_put_bytes(tty, achar(13))
        call tty_flush(tty, status)
    end subroutine tty_reset_terminal_modes

    !> Port of term.c:tty_update_end.
    subroutine tty_update_end(tty, cursor_off)
        type(tty_display_info), intent(inout) :: tty
        logical, intent(in) :: cursor_off
        type(status_type) :: status

        if (.not. cursor_off) call tty_show_cursor(tty)
        call tty_turn_off_insert(tty)
        call tty_background_highlight(tty)
        call tty_flush(tty, status)
    end subroutine tty_update_end

    !> Limit scrolling to the first `size' lines (0: all).  Port of
    !> term.c:tty_set_terminal_window.
    subroutine tty_set_terminal_window(tty, size)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: size

        tty%specified_window = size
        if (size == 0) tty%specified_window = tty%wcm%cm_rows
        if (tty%scroll_region_ok) call tty_set_scroll_region(tty, 0, tty%specified_window)
    end subroutine tty_set_terminal_window

    !> Port of term.c:tty_set_scroll_region (lines `start'..`stop'-1).
    subroutine tty_set_scroll_region(tty, start, stop)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: start, stop
        character(len=:), allocatable :: buf
        integer(int64) :: rows

        rows = tty%wcm%cm_rows

        if (allocated(tty%ts_set_scroll_region)) then
            buf = tparam(tty%ts_set_scroll_region, int(start, int64), int(stop - 1, int64), &
                         0_int64, 0_int64)
        else if (allocated(tty%ts_set_scroll_region_1)) then
            buf = tparam(tty%ts_set_scroll_region_1, rows, int(start, int64), rows - stop, rows)
        else if (allocated(tty%ts_set_window)) then
            buf = tparam(tty%ts_set_window, int(start, int64), 0_int64, int(stop, int64), &
                         int(tty%wcm%cm_cols, int64))
        else
            return
        end if

        call tty_output(tty, buf)
        call losecursor(tty)
    end subroutine tty_set_scroll_region

    !> Port of term.c:tty_turn_on_insert.
    subroutine tty_turn_on_insert(tty)
        type(tty_display_info), intent(inout) :: tty

        if (.not. tty%insert_mode) call tty_output(tty, tty%ts_insert_mode)
        tty%insert_mode = .true.
    end subroutine tty_turn_on_insert

    !> Port of term.c:tty_turn_off_insert.
    subroutine tty_turn_off_insert(tty)
        type(tty_display_info), intent(inout) :: tty

        if (tty%insert_mode) call tty_output(tty, tty%ts_end_insert_mode)
        tty%insert_mode = .false.
    end subroutine tty_turn_off_insert

    !> Port of term.c:tty_turn_off_highlight.
    subroutine tty_turn_off_highlight(tty)
        type(tty_display_info), intent(inout) :: tty

        if (tty%standout_mode) call tty_output(tty, tty%ts_end_standout_mode)
        tty%standout_mode = .false.
    end subroutine tty_turn_off_highlight

    !> Port of term.c:tty_turn_on_highlight.
    subroutine tty_turn_on_highlight(tty)
        type(tty_display_info), intent(inout) :: tty

        if (.not. tty%standout_mode) call tty_output(tty, tty%ts_standout_mode)
        tty%standout_mode = .true.
    end subroutine tty_turn_on_highlight

    !> Port of term.c:tty_toggle_highlight.
    subroutine tty_toggle_highlight(tty)
        type(tty_display_info), intent(inout) :: tty

        if (tty%standout_mode) then
            call tty_turn_off_highlight(tty)
        else
            call tty_turn_on_highlight(tty)
        end if
    end subroutine tty_toggle_highlight

    !> Port of term.c:tty_hide_cursor.
    subroutine tty_hide_cursor(tty)
        type(tty_display_info), intent(inout) :: tty

        if (tty%cursor_hidden) return
        tty%cursor_hidden = .true.
        call tty_output(tty, tty%ts_cursor_invisible)
    end subroutine tty_hide_cursor

    !> Port of term.c:tty_show_cursor.
    subroutine tty_show_cursor(tty)
        type(tty_display_info), intent(inout) :: tty

        if (.not. tty%cursor_hidden) return
        tty%cursor_hidden = .false.
        call tty_output(tty, tty%ts_cursor_normal)
        if (visible_cursor) call tty_output(tty, tty%ts_cursor_visible)
    end subroutine tty_show_cursor

    !> Port of term.c:tty_background_highlight (also
    !> tty_highlight_if_desired, which is identical).
    subroutine tty_background_highlight(tty)
        type(tty_display_info), intent(inout) :: tty

        if (inverse_video) then
            call tty_turn_on_highlight(tty)
        else
            call tty_turn_off_highlight(tty)
        end if
    end subroutine tty_background_highlight

    ! ------------------------------------------------------------------
    ! Cursor motion and clearing
    ! ------------------------------------------------------------------
    !> Move the cursor to row `vpos', column `hpos' (0-based).  Port of
    !> term.c:tty_cursor_to.
    subroutine tty_cursor_to(tty, vpos, hpos)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: vpos, hpos

        if (.not. tty%costs_set) return
        if (tty%wcm%cm_cury == vpos .and. tty%wcm%cm_curx == hpos) return
        if (.not. tty%tf_standout_motion) call tty_background_highlight(tty)
        if (.not. tty%tf_insmode_motion) call tty_turn_off_insert(tty)
        call cmgoto(tty, vpos, hpos)
    end subroutine tty_cursor_to

    !> Like tty_cursor_to, in absolute terminal coordinates.  Port of
    !> term.c:tty_raw_cursor_to.
    subroutine tty_raw_cursor_to(tty, row, col)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: row, col

        if (tty%wcm%cm_cury == row .and. tty%wcm%cm_curx == col) return
        if (.not. tty%tf_standout_motion) call tty_background_highlight(tty)
        if (.not. tty%tf_insmode_motion) call tty_turn_off_insert(tty)
        call cmgoto(tty, row, col)
    end subroutine tty_raw_cursor_to

    !> Clear from the cursor to the end of the screen.  Port of
    !> term.c:tty_clear_to_end.
    subroutine tty_clear_to_end(tty)
        type(tty_display_info), intent(inout) :: tty
        integer :: i

        if (allocated(tty%ts_clr_to_bottom)) then
            call tty_background_highlight(tty)
            call tty_output(tty, tty%ts_clr_to_bottom)
        else
            do i = tty%wcm%cm_cury, tty%wcm%cm_rows - 1
                call tty_cursor_to(tty, i, 0)
                call tty_clear_end_of_line(tty, tty%wcm%cm_cols)
            end do
        end if
    end subroutine tty_clear_to_end

    !> Clear the whole screen.  Port of term.c:tty_clear_frame.
    subroutine tty_clear_frame(tty)
        type(tty_display_info), intent(inout) :: tty

        if (allocated(tty%ts_clr_frame)) then
            call tty_background_highlight(tty)
            call tty_output(tty, tty%ts_clr_frame)
            call cmat(tty, 0, 0)
        else
            call tty_cursor_to(tty, 0, 0)
            call tty_clear_to_end(tty)
        end if
    end subroutine tty_clear_frame

    !> Clear from the cursor to column `first_unused_hpos' (the end of
    !> what was written).  Port of term.c:tty_clear_end_of_line.
    subroutine tty_clear_end_of_line(tty, first_unused_hpos)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: first_unused_hpos
        integer :: last

        if (.not. tty%costs_set) return
        if (tty%wcm%cm_curx >= first_unused_hpos) return
        call tty_background_highlight(tty)

        if (allocated(tty%ts_clr_line)) then
            call tty_output(tty, tty%ts_clr_line)
        else
            ! No clear-line: write spaces, but never in the last cell of an
            ! autowrapping terminal.
            call tty_turn_off_insert(tty)
            last = first_unused_hpos
            if (tty%wcm%cm_autowrap .and. tty%wcm%cm_cury == tty%wcm%cm_rows - 1 &
                .and. last == tty%wcm%cm_cols) last = last - 1

            if (last > tty%wcm%cm_curx) then
                call tty_put_bytes(tty, repeat(" ", last - tty%wcm%cm_curx))
                call cmplus(tty, last - tty%wcm%cm_curx)
            end if
        end if
    end subroutine tty_clear_end_of_line

    ! ------------------------------------------------------------------
    ! Glyphs
    ! ------------------------------------------------------------------
    !> The bytes that show `glyphs' on a UTF-8 terminal; padding glyphs
    !> (the rest of a wide character) produce nothing.  Port of
    !> term.c:encode_terminal_code for the UTF-8 terminal coding system.
    function encode_terminal_code(glyphs) result(bytes)
        type(glyph_type), intent(in) :: glyphs(:)
        character(len=:), allocatable :: bytes
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: n
        type(status_type) :: status
        integer :: k

        bytes = ""

        do k = 1, size(glyphs)
            if (glyphs(k)%padding) cycle

            if (char_byte8_p(glyphs(k)%ch)) then
                bytes = bytes//char(char_to_byte8(glyphs(k)%ch))
            else
                call char_string(glyphs(k)%ch, buf, n, status)

                if (status%ok()) then
                    bytes = bytes//buf(1:n)
                else
                    bytes = bytes//"?"
                end if
            end if
        end do
    end function encode_terminal_code

    !> Whether attribute `attr' may be combined with colors.
    !> term.c's MAY_USE_WITH_COLORS_P.
    logical function may_use_with_colors(tty, attr)
        type(tty_display_info), intent(in) :: tty
        integer, intent(in) :: attr

        may_use_with_colors = .true.
        if (tty%tn_max_colors > 0) may_use_with_colors = iand(tty%tn_no_color_video, attr) == 0
    end function may_use_with_colors

    !> Return true if the terminal TTY is capable of displaying all the
    !> attributes in CAPS (TTY_CAP_* bits), else false.  Port of
    !> term.c:tty_capable_p.
    logical function tty_capable_p(tty, caps) result(capable)
        type(tty_display_info), intent(in) :: tty
        integer, intent(in) :: caps

        capable = .false.
        if (.not. try(TTY_CAP_INVERSE, allocated(tty%ts_standout_mode), NC_REVERSE)) return
        if (.not. try(TTY_CAP_UNDERLINE, allocated(tty%ts_enter_underline_mode), NC_UNDERLINE)) return
        if (.not. try(TTY_CAP_UNDERLINE_STYLED, allocated(tty%tf_set_underline_style), NC_UNDERLINE)) return
        if (.not. try(TTY_CAP_BOLD, allocated(tty%ts_enter_bold_mode), NC_BOLD)) return
        if (.not. try(TTY_CAP_DIM, allocated(tty%ts_enter_dim_mode), NC_DIM)) return
        if (.not. try(TTY_CAP_ITALIC, allocated(tty%ts_enter_italic_mode), NC_ITALIC)) return
        if (.not. try(TTY_CAP_STRIKE_THROUGH, allocated(tty%ts_enter_strike_through_mode), NC_STRIKE_THROUGH)) return
        ! We can do it!
        capable = .true.

    contains

        !> TTY_CAPABLE_P_TRY: false if CAP is asked for and the terminal
        !> lacks it (HAVE) or can't use it with colors.
        logical function try(cap, have, nc_bit)
            integer, intent(in) :: cap, nc_bit
            logical, intent(in) :: have

            try = .true.
            if (iand(caps, cap) /= 0) try = have .and. may_use_with_colors(tty, nc_bit)
        end function try
    end function tty_capable_p

    !> Output a color capability for `color'.
    subroutine set_color(tty, cap, color)
        type(tty_display_info), intent(inout) :: tty
        character(len=:), allocatable, intent(in) :: cap
        integer(int64), intent(in) :: color
        character(len=:), allocatable :: p

        if (.not. allocated(cap)) return

        if (tty%tf_rgb_separate) then
            p = tparam(cap, ishft(color, -16), iand(ishft(color, -8), 255_int64), &
                       iand(color, 255_int64), 0_int64)
        else
            p = tparam(cap, color, 0_int64, 0_int64, 0_int64)
        end if

        call tty_output(tty, p)
    end subroutine set_color

    !> Start drawing in `face'.  Port of term.c:turn_on_face.
    subroutine turn_on_face(tty, face)
        type(tty_display_info), intent(inout) :: tty
        type(face_type), intent(in) :: face
        integer(int64) :: fg, bg
        character(len=:), allocatable :: p

        fg = face%foreground
        bg = face%background

        if (may_use_with_colors(tty, NC_REVERSE)) then
            if (inverse_video) then
                if (fg == FACE_TTY_DEFAULT_FG_COLOR .or. bg == FACE_TTY_DEFAULT_BG_COLOR) then
                    call tty_toggle_highlight(tty)
                end if
            else
                if (fg == FACE_TTY_DEFAULT_BG_COLOR .or. bg == FACE_TTY_DEFAULT_FG_COLOR) then
                    call tty_toggle_highlight(tty)
                end if
            end if
        end if

        if (face%tty_bold_p .and. may_use_with_colors(tty, NC_BOLD)) then
            call tty_output(tty, tty%ts_enter_bold_mode)
        end if

        if (face%tty_italic_p .and. may_use_with_colors(tty, NC_ITALIC)) then
            if (allocated(tty%ts_enter_italic_mode)) then
                call tty_output(tty, tty%ts_enter_italic_mode)
            else
                call tty_output(tty, tty%ts_enter_dim_mode)
            end if
        end if

        if (face%underline /= FACE_NO_UNDERLINE .and. may_use_with_colors(tty, NC_UNDERLINE)) then
            if (face%underline == FACE_UNDERLINE_SINGLE .or. &
                .not. allocated(tty%tf_set_underline_style)) then
                call tty_output(tty, tty%ts_enter_underline_mode)
            else
                p = tparam(tty%tf_set_underline_style, int(face%underline, int64), 0_int64, &
                           0_int64, 0_int64)
                call tty_output(tty, p)
            end if
        end if

        if (face%tty_strike_through_p .and. may_use_with_colors(tty, NC_STRIKE_THROUGH)) then
            call tty_output(tty, tty%ts_enter_strike_through_mode)
        end if

        if (tty%tn_max_colors > 0) then
            ! In standout mode foreground and background are swapped.
            if (face_tty_specified_color(fg)) then
                if (tty%standout_mode) then
                    call set_color(tty, tty%ts_set_background, fg)
                else
                    call set_color(tty, tty%ts_set_foreground, fg)
                end if
            end if

            if (face_tty_specified_color(bg)) then
                if (tty%standout_mode) then
                    call set_color(tty, tty%ts_set_foreground, bg)
                else
                    call set_color(tty, tty%ts_set_background, bg)
                end if
            end if

            if (allocated(tty%tf_set_underline_color) .and. face%underline_color /= 0) then
                p = tparam(tty%tf_set_underline_color, face%underline_color, 0_int64, 0_int64, 0_int64)
                call tty_output(tty, p)
            end if
        end if
    end subroutine turn_on_face

    !> Stop drawing in `face'.  Port of term.c:turn_off_face.
    subroutine turn_off_face(tty, face)
        type(tty_display_info), intent(inout) :: tty
        type(face_type), intent(in) :: face

        if (allocated(tty%ts_exit_attribute_mode)) then
            if (face%tty_bold_p .or. face%tty_italic_p .or. face%tty_reverse_p .or. &
                face%underline /= FACE_NO_UNDERLINE .or. face%tty_strike_through_p) then
                call tty_output(tty, tty%ts_exit_attribute_mode)

                if (allocated(tty%ts_end_standout_mode)) then
                    if (tty%ts_exit_attribute_mode == tty%ts_end_standout_mode) then
                        tty%standout_mode = .false.
                    end if
                end if
            end if
        else if (face%underline /= FACE_NO_UNDERLINE) then
            call tty_output(tty, tty%ts_exit_underline_mode)
        end if

        if (tty%tn_max_colors > 0) then
            if ((face%foreground /= FACE_TTY_DEFAULT_COLOR .and. &
                 face%foreground /= FACE_TTY_DEFAULT_FG_COLOR) .or. &
                (face%background /= FACE_TTY_DEFAULT_COLOR .and. &
                 face%background /= FACE_TTY_DEFAULT_BG_COLOR)) then
                call tty_output(tty, tty%ts_orig_pair)
            end if
        end if
    end subroutine turn_off_face

    !> The face for `face_id', or the default face if out of range.
    function face_of(faces, face_id) result(face)
        type(face_type), intent(in) :: faces(0:)
        integer(int32), intent(in) :: face_id
        type(face_type) :: face

        if (face_id >= 0 .and. face_id <= ubound(faces, 1)) face = faces(face_id)
    end function face_of

    !> Write `glyphs' at the cursor, in runs of the same face.  Port of
    !> term.c:tty_write_glyphs_1.
    subroutine tty_write_glyphs_1(tty, glyphs, faces)
        type(tty_display_info), intent(inout) :: tty
        type(glyph_type), intent(in) :: glyphs(:)
        type(face_type), intent(in) :: faces(0:)
        type(face_type) :: face
        integer :: start, n, len_

        call tty_turn_off_insert(tty)
        call tty_hide_cursor(tty)
        len_ = size(glyphs)
        if (len_ == 0) return
        call cmplus(tty, len_)
        start = 1

        do while (start <= len_)
            n = 1

            do while (start + n <= len_)
                if (glyphs(start + n)%face_id /= glyphs(start)%face_id) exit
                n = n + 1
            end do

            call tty_background_highlight(tty)
            face = face_of(faces, glyphs(start)%face_id)
            call turn_on_face(tty, face)
            call tty_put_bytes(tty, encode_terminal_code(glyphs(start:start + n - 1)))
            call turn_off_face(tty, face)
            call tty_turn_off_highlight(tty)
            start = start + n
        end do

        call cmcheckmagic(tty)
    end subroutine tty_write_glyphs_1

    !> Write `glyphs' at the cursor.  Writing the last cell of an
    !> autowrapping terminal would scroll it, so that cell is inserted
    !> instead; when only that cell is written, `prev' (the glyph before
    !> it, which upstream reads from the glyph row) is rewritten too, and
    !> without it the last cell is left alone.  Port of
    !> term.c:tty_write_glyphs.
    recursive subroutine tty_write_glyphs(tty, glyphs, faces, prev)
        type(tty_display_info), intent(inout) :: tty
        type(glyph_type), intent(in) :: glyphs(:)
        type(face_type), intent(in) :: faces(0:)
        type(glyph_type), intent(in), optional :: prev
        type(glyph_type), allocatable :: run(:)
        integer :: len_, old_x, old_y

        len_ = size(glyphs)

        if (tty%wcm%cm_autowrap .and. tty%wcm%cm_cury + 1 == tty%wcm%cm_rows .and. &
            tty%wcm%cm_curx + len_ == tty%wcm%cm_cols .and. len_ > 0) then
            if (len_ == 1) then
                if (.not. present(prev) .or. tty%wcm%cm_curx == 0) return
                call cmgoto(tty, tty%wcm%cm_cury, tty%wcm%cm_curx - 1)
                allocate (run(2))
                run(1) = prev
                run(2) = glyphs(1)
            else
                run = glyphs
            end if

            old_x = tty%wcm%cm_curx
            old_y = tty%wcm%cm_cury
            call tty_write_glyphs_1(tty, run(2:), faces)
            call cmgoto(tty, old_y, old_x)
            call tty_insert_glyphs(tty, faces, run(1:1))
        else
            call tty_write_glyphs_1(tty, glyphs, faces)
        end if
    end subroutine tty_write_glyphs

    !> Insert `len' blanks, or `glyphs' when given, at the cursor.  Port of
    !> term.c:tty_insert_glyphs.
    recursive subroutine tty_insert_glyphs(tty, faces, glyphs, len)
        type(tty_display_info), intent(inout) :: tty
        type(face_type), intent(in) :: faces(0:)
        type(glyph_type), intent(in), optional :: glyphs(:)
        integer, intent(in), optional :: len
        type(face_type) :: face
        character(len=:), allocatable :: buf
        integer :: n, k

        if (present(glyphs)) then
            n = size(glyphs)
        else
            n = 0
            if (present(len)) n = len
        end if

        if (n <= 0) return

        if (allocated(tty%ts_ins_multi_chars)) then
            buf = tparam(tty%ts_ins_multi_chars, int(n, int64), 0_int64, 0_int64, 0_int64)
            call tty_output(tty, buf)
            if (present(glyphs)) call tty_write_glyphs(tty, glyphs, faces)
            return
        end if

        call tty_turn_on_insert(tty)
        call cmplus(tty, n)
        k = 1

        do while (k <= n)
            call tty_output(tty, tty%ts_ins_char)

            if (.not. present(glyphs)) then
                call tty_put_bytes(tty, " ")
                call tty_output(tty, tty%ts_pad_inserted_char)
                k = k + 1
                cycle
            end if

            call tty_background_highlight(tty)
            face = face_of(faces, glyphs(k)%face_id)
            call turn_on_face(tty, face)
            call tty_put_bytes(tty, encode_terminal_code(glyphs(k:k)))
            k = k + 1

            ! The padding cells of a wide character need room too.
            do while (k <= n)
                if (.not. glyphs(k)%padding) exit
                call tty_output(tty, tty%ts_ins_char)
                k = k + 1
            end do

            call tty_output(tty, tty%ts_pad_inserted_char)
            call turn_off_face(tty, face)
            call tty_turn_off_highlight(tty)
        end do

        call cmcheckmagic(tty)
    end subroutine tty_insert_glyphs

    !> Delete `n' characters at the cursor.  Port of term.c:tty_delete_glyphs.
    subroutine tty_delete_glyphs(tty, n)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: n
        character(len=:), allocatable :: buf
        integer :: i

        if (tty%delete_in_insert_mode) then
            call tty_turn_on_insert(tty)
        else
            call tty_turn_off_insert(tty)
            call tty_output(tty, tty%ts_delete_mode)
        end if

        if (allocated(tty%ts_del_multi_chars)) then
            buf = tparam(tty%ts_del_multi_chars, int(n, int64), 0_int64, 0_int64, 0_int64)
            call tty_output(tty, buf)
        else
            do i = 1, n
                call tty_output(tty, tty%ts_del_char)
            end do
        end if

        if (.not. tty%delete_in_insert_mode) call tty_output(tty, tty%ts_end_delete_mode)
    end subroutine tty_delete_glyphs

    !> Insert `n' blank lines at row `vpos' (delete -`n' lines if
    !> negative), within the scroll window.  Port of term.c:tty_ins_del_lines.
    subroutine tty_ins_del_lines(tty, vpos, n)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: vpos, n
        character(len=:), allocatable :: multi, single, scroll, buf
        integer :: i, k

        if (n > 0) then
            if (allocated(tty%ts_ins_multi_lines)) multi = tty%ts_ins_multi_lines
            if (allocated(tty%ts_ins_line)) single = tty%ts_ins_line
            if (allocated(tty%ts_rev_scroll)) scroll = tty%ts_rev_scroll
        else
            if (allocated(tty%ts_del_multi_lines)) multi = tty%ts_del_multi_lines
            if (allocated(tty%ts_del_line)) single = tty%ts_del_line
            if (allocated(tty%ts_fwd_scroll)) scroll = tty%ts_fwd_scroll
        end if

        i = abs(n)
        if (tty%scroll_region_ok .and. vpos + i >= tty%specified_window) return
        if (.not. tty%memory_below_frame .and. vpos + i >= tty%wcm%cm_rows) return

        if (allocated(multi)) then
            call tty_raw_cursor_to(tty, vpos, 0)
            call tty_background_highlight(tty)
            buf = tparam(multi, int(i, int64), 0_int64, 0_int64, 0_int64)
            call tty_output(tty, buf)
        else if (allocated(single)) then
            call tty_raw_cursor_to(tty, vpos, 0)
            call tty_background_highlight(tty)

            do k = 1, i
                call tty_output(tty, single)
            end do

            if (tty%tf_teleray) tty%wcm%cm_curx = 0
        else
            call tty_set_scroll_region(tty, vpos, tty%specified_window)

            if (n < 0) then
                call tty_raw_cursor_to(tty, tty%specified_window - 1, 0)
            else
                call tty_raw_cursor_to(tty, vpos, 0)
            end if

            call tty_background_highlight(tty)

            do k = 1, i
                call tty_output(tty, scroll)
            end do

            call tty_set_scroll_region(tty, 0, tty%specified_window)
        end if

        if (.not. tty%scroll_region_ok .and. tty%memory_below_frame .and. n < 0) then
            call tty_cursor_to(tty, tty%wcm%cm_rows + n, 0)
            call tty_clear_to_end(tty)
        end if
    end subroutine tty_ins_del_lines
end module term_fm
