! terminal_output_mod.f90 -- terminal output: cursor motion, clearing, insertion and
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
!> terminal here, so they take the terminal_display_info_type and the frame's size
!> is the terminal's (FrameRows, FrameCols).  Glyph-writing functions take
!> the face table to draw with, indexed by face id from 0.
!>
!> Not ported: menus, mouse (GPM), glyph production (produce_glyphs, in
!> the redisplay step), color-mode switching (terminal_setup_colors),
!> tty-mode-set-strings, the termscript, character_ins_delete cost vectors, the
!> DOS/Windows/Android branches.
module terminal_output_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use value_mod, only: value_type, list
    use character_mod, only: downcase_ascii, MAXIMUM_MULTIBYTE_LENGTH, character_string, is_character_byte8, &
                             character_to_byte8
    use terminfo_mod, only: terminfo_type, tgetent, tgetstr, tgetnum, tgetflag, tigetstr, &
                            tigetflag, tparam, delete_padding
    use terminal_state_mod, only: terminal_display_info_type, terminal_output, terminal_put_bytes, terminal_flush
    use cursor_motion_mod, only: cmcostinit, cmgoto, cmcheckmagic, cmat, cmplus, &
                                 losecursor, wcm_initialize
    use glyph_matrices_mod, only: glyph_type, face_type, FACE_TERMINAL_DEFAULT_COLOR, &
                                  FACE_TERMINAL_DEFAULT_FOREGROUND_COLOR, FACE_TERMINAL_DEFAULT_BACKGROUND_COLOR, &
                                  FACE_NO_UNDERLINE, FACE_UNDERLINE_SINGLE, &
                                  face_terminal_specified_color, TERMINAL_CAP_INVERSE, TERMINAL_CAP_UNDERLINE, &
                                  TERMINAL_CAP_UNDERLINE_STYLED, TERMINAL_CAP_BOLD, TERMINAL_CAP_DIM, TERMINAL_CAP_ITALIC, &
                                  TERMINAL_CAP_STRIKE_THROUGH
    use keymap_mod, only: make_sparse_keymap, define_key, is_keymap_live
    use keyboard_mod, only: input_decode_map
    use coding_mod, only: terminal_coding_system, coding_system_type, encode_coding_string, is_character_encodable
    use system_dependent_mod, only: get_terminal_size, is_tabs_safe, initialize_system_modes, &
                                    reset_system_modes
    use frame_mod, only: selected_frame
    implicit none(type, external)
    private

    public :: initialize_terminal, initialize_terminal_entry, delete_terminal, calculate_costs, term_get_fkeys
    public :: reset_all_system_modes, initialize_all_system_modes
    public :: is_terminal_display_color, terminal_display_color_cells, terminal_type, is_controlling_terminal, terminal_no_underline
    public :: terminal_ring_bell, terminal_set_terminal_modes, terminal_reset_terminal_modes, &
              terminal_update_end, terminal_set_terminal_window, terminal_set_scroll_region, &
              terminal_turn_on_insert, terminal_turn_off_insert, terminal_turn_on_highlight, &
              terminal_turn_off_highlight, terminal_hide_cursor, terminal_show_cursor, &
              terminal_cursor_to, terminal_raw_cursor_to, terminal_clear_to_end, &
              terminal_clear_frame, terminal_clear_end_of_line, terminal_write_glyphs, &
              terminal_insert_glyphs, terminal_delete_glyphs, terminal_ins_delete_lines, &
              turn_on_face, turn_off_face, encode_terminal_code, is_terminal_capable

    !> True means try to flash the frame to represent a bell.
    !>
    !> See also `ring_bell_function'.  Port of dispnew.c visible-bell (with
    !> dispnew.c inverse-video and term.c visible-cursor, declared with it).
    logical, public :: visible_bell = .false., inverse_video = .false., &
                       visible_cursor = .true.

    !> True means redraw the entire frame after suspending.
    !> A false value is useful if the terminal can automatically preserve
    !> Fortmacs's frame display when you reenter Fortmacs.
    !> It is up to you to set this variable if your terminal can do that.
    !> Port of dispnew.c no-redraw-on-reenter (named for what it does, its
    !> value inverted).
    logical, public :: redraw_after_suspend = .true.

    !> term.c:enum no_color_bit (terminfo ncv).
    integer, parameter :: NC_STANDOUT = 1, NC_UNDERLINE = 2, NC_REVERSE = 4, &
                          NC_ITALIC = 8, NC_DIM = 16, NC_BOLD = 32, &
                          NC_STRIKE_THROUGH = 64

contains

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> `destination' = termcap string capability `code', unallocated if absent.
    !> Not a port.
    subroutine getcap(terminal, code, destination)
        type(terminal_display_info_type), intent(in) :: terminal
        character(len=*), intent(in) :: code
        character(len=:), allocatable, intent(out) :: destination
        character(len=:), allocatable :: s
        logical :: found

        s = tgetstr(terminal%ti, code, found)
        if (found) destination = s
    end subroutine getcap

    !> Like `getcap' with a terminfo name.  Not a port.
    subroutine getcap_ti(terminal, name, destination)
        type(terminal_display_info_type), intent(in) :: terminal
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: destination
        character(len=:), allocatable :: s
        logical :: found

        s = tigetstr(terminal%ti, name, found)
        if (found) destination = s
    end subroutine getcap_ti

    !> Set up `terminal' as terminal type `terminal_type' on file descriptor
    !> `fd' (-1: no terminal; the size then comes from the terminfo entry):
    !> read its capabilities, register its function keys in
    !> input-decode-map, put it in raw mode, and compute motion costs.
    !> Port of term.c:init_tty.
    subroutine initialize_terminal(terminal, terminal_type, fd, status)
        type(terminal_display_info_type), intent(out) :: terminal
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(terminfo_type) :: ti

        call tgetent(terminal_type, ti, status)
        if (.not. status%ok()) return
        call initialize_terminal_entry(terminal, ti, terminal_type, fd, status)
    end subroutine initialize_terminal

    !> initialize_terminal with the terminal description `ti' already loaded.
    !> Port of term.c:init_tty, in part.
    subroutine initialize_terminal_entry(terminal, ti, terminal_type, fd, status)
        type(terminal_display_info_type), intent(out) :: terminal
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: colorterm, foreground, background
        character(len=48) :: text
        integer(int32) :: width, height
        type(status_type) :: st
        integer :: length, stat

        terminal%type = terminal_type
        ! The controlling tty (initialize_terminal with NAME null: dev_terminal).
        terminal%name = "/dev/tty"
        terminal%input = fd
        terminal%output = fd
        terminal%ti = ti
        call getcap(terminal, "al", terminal%ts_ins_line)
        call getcap(terminal, "AL", terminal%ts_ins_multi_lines)
        call getcap(terminal, "bl", terminal%ts_bell)
        call getcap(terminal, "bt", terminal%wcm%cursor_motion_backtab)
        call getcap(terminal, "cd", terminal%ts_clr_to_bottom)
        call getcap(terminal, "ce", terminal%ts_clr_line)
        call getcap(terminal, "cl", terminal%ts_clr_frame)
        ! Column-absolute motion ("ch") stays unused, as upstream.
        call getcap(terminal, "cm", terminal%wcm%cursor_motion_absolute)
        call getcap(terminal, "cr", terminal%wcm%cursor_motion_cr)
        call getcap(terminal, "cs", terminal%ts_set_scroll_region)
        call getcap(terminal, "cS", terminal%ts_set_scroll_region_1)
        call getcap(terminal, "cv", terminal%wcm%cursor_motion_vabs)
        call getcap(terminal, "dc", terminal%ts_delete_character)
        call getcap(terminal, "DC", terminal%ts_delete_multi_characters)
        call getcap(terminal, "dl", terminal%ts_delete_line)
        call getcap(terminal, "DL", terminal%ts_delete_multi_lines)
        call getcap(terminal, "dm", terminal%ts_delete_mode)
        call getcap(terminal, "ed", terminal%ts_end_delete_mode)
        call getcap(terminal, "ei", terminal%ts_end_insert_mode)
        call getcap(terminal, "ho", terminal%wcm%cursor_motion_home)
        call getcap(terminal, "ic", terminal%ts_ins_character)
        call getcap(terminal, "IC", terminal%ts_ins_multi_characters)
        call getcap(terminal, "im", terminal%ts_insert_mode)
        call getcap(terminal, "ip", terminal%ts_pad_inserted_character)
        call getcap(terminal, "ke", terminal%ts_end_keypad_mode)
        call getcap(terminal, "ks", terminal%ts_keypad_mode)
        call getcap(terminal, "ll", terminal%wcm%cursor_motion_ll)
        call getcap(terminal, "nd", terminal%wcm%cursor_motion_right)
        call getcap(terminal, "do", terminal%wcm%cursor_motion_down)
        if (.not. allocated(terminal%wcm%cursor_motion_down)) call getcap(terminal, "nl", terminal%wcm%cursor_motion_down)

        if (tgetflag(terminal%ti, "bs")) then
            terminal%wcm%cursor_motion_left = achar(8)
        else
            call getcap(terminal, "le", terminal%wcm%cursor_motion_left)
        end if

        if (.not. allocated(terminal%wcm%cursor_motion_left)) call getcap(terminal, "bc", terminal%wcm%cursor_motion_left)
        call getcap(terminal, "pc", terminal%ts_pad_character)
        call getcap(terminal, "rp", terminal%ts_repeat)
        call getcap(terminal, "se", terminal%ts_end_standout_mode)
        call getcap(terminal, "sf", terminal%ts_fwd_scroll)
        call getcap(terminal, "so", terminal%ts_standout_mode)
        call getcap(terminal, "sr", terminal%ts_revision_scroll)
        call getcap(terminal, "ta", terminal%wcm%cursor_motion_tab)
        call getcap(terminal, "te", terminal%ts_end_termcap_modes)
        call getcap(terminal, "ti", terminal%ts_termcap_modes)
        call getcap(terminal, "up", terminal%wcm%cursor_motion_up)
        call getcap(terminal, "vb", terminal%ts_visible_bell)
        call getcap(terminal, "ve", terminal%ts_cursor_normal)
        call getcap(terminal, "vs", terminal%ts_cursor_visible)
        call getcap(terminal, "vi", terminal%ts_cursor_invisible)
        call getcap(terminal, "wi", terminal%ts_set_window)
        call getcap(terminal, "us", terminal%ts_enter_underline_mode)
        call getcap(terminal, "ue", terminal%ts_exit_underline_mode)
        call getcap(terminal, "md", terminal%ts_enter_bold_mode)
        call getcap(terminal, "ZH", terminal%ts_enter_italic_mode)
        call getcap(terminal, "mh", terminal%ts_enter_dim_mode)
        call getcap(terminal, "mr", terminal%ts_enter_reverse_mode)
        call getcap(terminal, "as", terminal%ts_enter_alt_character_set_mode)
        call getcap(terminal, "ae", terminal%ts_exit_alt_character_set_mode)
        call getcap(terminal, "me", terminal%ts_exit_attribute_mode)
        call getcap_ti(terminal, "smxx", terminal%ts_enter_strike_through_mode)
        call getcap(terminal, "UP", terminal%wcm%cursor_motion_multiup)
        call getcap(terminal, "DO", terminal%wcm%cursor_motion_multidown)
        call getcap(terminal, "LE", terminal%wcm%cursor_motion_multileft)
        call getcap(terminal, "RI", terminal%wcm%cursor_motion_multiright)
        ! Colors.
        call getcap(terminal, "op", terminal%ts_original_pair)

        if (allocated(terminal%ts_original_pair)) then
            call getcap(terminal, "AF", terminal%ts_set_foreground)
            call getcap(terminal, "AB", terminal%ts_set_background)

            if (.not. allocated(terminal%ts_set_foreground)) then
                call getcap(terminal, "Sf", terminal%ts_set_foreground)
                call getcap(terminal, "Sb", terminal%ts_set_background)
            end if

            terminal%tn_maximum_colors = tgetnum(terminal%ti, "Co")
            call getcap_ti(terminal, "setf24", foreground)
            call getcap_ti(terminal, "setb24", background)

            if (allocated(foreground) .and. allocated(background)) then
                terminal%ts_set_foreground = foreground
                terminal%ts_set_background = background
                terminal%tn_maximum_colors = 16777216
            else
                call getcap_ti(terminal, "setrgbf", foreground)
                call getcap_ti(terminal, "setrgbb", background)
                colorterm = ""
                call get_environment_variable("COLORTERM", length=length, status=stat)

                if (stat == 0 .and. length > 0) then
                    deallocate (colorterm)
                    allocate (character(len=length) :: colorterm)
                    call get_environment_variable("COLORTERM", colorterm)
                end if

                if (allocated(foreground) .and. allocated(background)) then
                    terminal%ts_set_foreground = foreground
                    terminal%ts_set_background = background
                    terminal%tn_maximum_colors = 16777216
                    terminal%tf_rgb_separate = .true.
                else if (tigetflag(terminal%ti, "RGB")) then
                    terminal%tn_maximum_colors = 16777216
                else if (tigetflag(terminal%ti, "Tc") .or. downcase_ascii(colorterm) == "truecolor") then
                    terminal%ts_set_foreground = achar(27)//"[38;2;%p1%d;%p2%d;%p3%d%;m"
                    terminal%ts_set_background = achar(27)//"[48;2;%p1%d;%p2%d;%p3%d%;m"
                    terminal%tn_maximum_colors = 16777216
                    terminal%tf_rgb_separate = .true.
                end if
            end if

            terminal%tn_no_color_video = tgetnum(terminal%ti, "NC")
            if (terminal%tn_no_color_video == -1) terminal%tn_no_color_video = 0
        end if

        terminal%wcm%cursor_motion_magicwrap = tgetflag(terminal%ti, "xn")
        terminal%wcm%cursor_motion_autowrap = terminal%wcm%cursor_motion_magicwrap .or. tgetflag(terminal%ti, "am")
        terminal%memory_below_frame = tgetflag(terminal%ti, "db")
        terminal%tf_hazeltine = tgetflag(terminal%ti, "hz")
        terminal%must_write_spaces = tgetflag(terminal%ti, "in")
        terminal%meta_key = tgetflag(terminal%ti, "km") .or. tgetflag(terminal%ti, "MT")
        terminal%tf_insmode_motion = tgetflag(terminal%ti, "mi")
        terminal%tf_standout_motion = tgetflag(terminal%ti, "ms")
        terminal%tf_underscore = tgetflag(terminal%ti, "ul")
        terminal%tf_teleray = tgetflag(terminal%ti, "xt")
        call getcap_ti(terminal, "Smulx", terminal%tf_set_underline_style)

        if (.not. allocated(terminal%tf_set_underline_style) .and. tgetflag(terminal%ti, "Su")) then
            terminal%tf_set_underline_style = achar(27)//"[4:%p1%dm"
        end if

        if (allocated(terminal%tf_set_underline_style)) then
            terminal%tf_set_underline_color = achar(27)// &
                                              "[58:2::%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm"
        end if

        call term_get_fkeys(terminal)
        width = 0
        height = 0
        if (fd >= 0) call get_terminal_size(fd, width, height, st)
        terminal%wcm%cursor_motion_columns = width
        terminal%wcm%cursor_motion_rows = height
        if (terminal%wcm%cursor_motion_columns <= 0) terminal%wcm%cursor_motion_columns = tgetnum(terminal%ti, "co")
        if (terminal%wcm%cursor_motion_rows <= 0) terminal%wcm%cursor_motion_rows = tgetnum(terminal%ti, "li")

        if (terminal%wcm%cursor_motion_rows < 3 .or. terminal%wcm%cursor_motion_columns < 3) then
            write (text, '("Screen size ", i0, "x", i0, " is too small")') terminal%wcm%cursor_motion_columns, &
                terminal%wcm%cursor_motion_rows
            call set_error(status, GENERIC_ERROR_CODE, trim(text))
            return
        end if

        terminal%wcm%cursor_motion_tabwidth = tgetnum(terminal%ti, "tw")
        if (.not. allocated(terminal%ts_bell)) terminal%ts_bell = achar(7)

        if (.not. allocated(terminal%ts_fwd_scroll) .and. allocated(terminal%wcm%cursor_motion_down)) then
            terminal%ts_fwd_scroll = terminal%wcm%cursor_motion_down
        end if

        if (terminal%wcm%cursor_motion_tabwidth < 0) terminal%wcm%cursor_motion_tabwidth = 8

        ! Standout or underline that take up a cell are not usable.
        if (allocated(terminal%ts_standout_mode) .and. tgetnum(terminal%ti, "sg") >= 0) then
            deallocate (terminal%ts_standout_mode)
            if (allocated(terminal%ts_end_standout_mode)) deallocate (terminal%ts_end_standout_mode)
        end if

        if (allocated(terminal%ts_enter_underline_mode) .and. tgetnum(terminal%ti, "ug") >= 0) then
            deallocate (terminal%ts_enter_underline_mode)
            if (allocated(terminal%ts_exit_underline_mode)) deallocate (terminal%ts_exit_underline_mode)
        end if

        ! No standout: use underline instead.
        if (.not. allocated(terminal%ts_standout_mode)) then
            if (allocated(terminal%ts_enter_underline_mode)) then
                terminal%ts_standout_mode = terminal%ts_enter_underline_mode
            end if

            if (allocated(terminal%ts_exit_underline_mode)) then
                terminal%ts_end_standout_mode = terminal%ts_exit_underline_mode
            else if (allocated(terminal%ts_end_standout_mode)) then
                deallocate (terminal%ts_end_standout_mode)
            end if
        end if

        ! No end of standout: use the end of all attributes.
        if (.not. allocated(terminal%ts_end_standout_mode)) then
            if (allocated(terminal%ts_exit_attribute_mode)) then
                terminal%ts_end_standout_mode = terminal%ts_exit_attribute_mode
            else if (allocated(terminal%ts_standout_mode)) then
                deallocate (terminal%ts_standout_mode)
            end if
        end if

        ! Teleray: tabs, CR and LF can't be trusted.
        if (terminal%tf_teleray) then
            if (allocated(terminal%wcm%cursor_motion_tab)) deallocate (terminal%wcm%cursor_motion_tab)
            if (allocated(terminal%ts_standout_mode)) deallocate (terminal%ts_standout_mode)
            if (allocated(terminal%wcm%cursor_motion_cr)) deallocate (terminal%wcm%cursor_motion_cr)
            if (allocated(terminal%wcm%cursor_motion_down)) deallocate (terminal%wcm%cursor_motion_down)
        end if

        terminal%specified_window = terminal%wcm%cursor_motion_rows

        if (wcm_initialize(terminal) == -1) then
            ! term.c:maybe_fatal: the error goes back to the caller.
            call set_error(status, GENERIC_ERROR_CODE, 'Terminal type "'//terminal_type// &
                           '" is not powerful enough to run Fortmacs')
            return
        end if

        terminal%delete_in_insert_mode = .false.

        if (allocated(terminal%ts_delete_mode) .and. allocated(terminal%ts_insert_mode)) then
            terminal%delete_in_insert_mode = terminal%ts_delete_mode == terminal%ts_insert_mode
        end if

        terminal%wcm%cursor_motion_usetabs = .false.
        if (fd >= 0) terminal%wcm%cursor_motion_usetabs = is_tabs_safe(fd) .and. terminal%wcm%cursor_motion_tabwidth == 8
        terminal%scroll_region_ok = allocated(terminal%wcm%cursor_motion_absolute) .and. &
                                    (allocated(terminal%ts_set_window) .or. allocated(terminal%ts_set_scroll_region) &
                                     .or. allocated(terminal%ts_set_scroll_region_1))
        terminal%line_ins_delete_ok = ((allocated(terminal%ts_ins_line) .or. allocated(terminal%ts_ins_multi_lines)) &
                                       .and. (allocated(terminal%ts_delete_line) .or. allocated(terminal%ts_delete_multi_lines))) &
                                      .or. (terminal%scroll_region_ok .and. allocated(terminal%ts_fwd_scroll) &
                                            .and. allocated(terminal%ts_revision_scroll))
        terminal%character_ins_delete_ok = (allocated(terminal%ts_ins_character) .or. allocated(terminal%ts_insert_mode) &
                                            .or. allocated(terminal%ts_pad_inserted_character) &
                                            .or. allocated(terminal%ts_ins_multi_characters)) &
                                           .and. (allocated(terminal%ts_delete_character) .or. allocated( &
                                                  terminal%ts_delete_multi_characters))

        if (fd >= 0) then
            call initialize_system_modes(fd, terminal%old_terminal, st, meta_key=terminal%meta_key)
            terminal%modes_saved = st%ok()
        end if

        call calculate_costs(terminal)
    end subroutine initialize_terminal_entry

    !> Restore the terminal: its modes as Fortmacs found them.  Port of
    !> term.c:delete_tty (with the reset_system_modes it triggers).
    subroutine delete_terminal(terminal, status)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(status_type), intent(out) :: status

        call terminal_reset_terminal_modes(terminal)
        call terminal_flush(terminal, status)

        if (terminal%modes_saved .and. terminal%input >= 0) then
            call reset_system_modes(terminal%input, terminal%old_terminal, status)
            terminal%modes_saved = .false.
        end if
    end subroutine delete_terminal

    !> Prepare all terminal devices for exiting Fortmacs: on the terminal of
    !> `selected_frame', go to and clear its last line, leave full-screen
    !> mode (`terminal_reset_terminal_modes') and give it back the modes Fortmacs
    !> found it in.  Port of sysdep.c:reset_all_sys_modes (one terminal)
    !> with sysdep.c:reset_sys_modes (its terminal part; system_dependent_mod's
    !> `reset_system_modes' restores the modes).
    subroutine reset_all_system_modes()
        type(status_type) :: status

        associate (terminal => selected_frame%terminal)
            ! Go to and clear the last line of the terminal.
            call cmgoto(terminal, terminal%wcm%cursor_motion_rows - 1, 0)

            if (allocated(terminal%ts_clr_line)) then
                call terminal_output(terminal, terminal%ts_clr_line)
            else
                ! Have to do it the hard way.
                call terminal_turn_off_insert(terminal)
                call terminal_put_bytes(terminal, repeat(" ", max(0, &
                                                         terminal%wcm%cursor_motion_columns - 1 - terminal%wcm%cursor_motion_curx)))
            end if

            call cmgoto(terminal, terminal%wcm%cursor_motion_rows - 1, 0)
            call terminal_flush(terminal, status)
            call terminal_reset_terminal_modes(terminal)

            if (terminal%modes_saved .and. terminal%input >= 0) then
                call reset_system_modes(terminal%input, terminal%old_terminal, status)
                terminal%modes_saved = .false.
            end if
        end associate
    end subroutine reset_all_system_modes

    !> Initialize the terminal mode on all tty devices that are currently
    !> open: the terminal of `selected_frame' gets Fortmacs's modes again
    !> (saving the ones it has), enters full-screen mode
    !> (`terminal_set_terminal_modes') and its frame is redrawn unless
    !> `redraw_after_suspend' is false.  Port of sysdep.c:init_all_sys_modes
    !> (one terminal) with sysdep.c:init_sys_modes (its terminal part;
    !> system_dependent_mod's `initialize_system_modes' sets the modes; it runs only when
    !> Fortmacs is resumed, so the terminal is always initialized already
    !> (term_initted); `terminal_erase_character' is not ported).
    subroutine initialize_all_system_modes()
        type(status_type) :: status

        associate (terminal => selected_frame%terminal)
            if (terminal%input >= 0) then
                call initialize_system_modes(terminal%input, terminal%old_terminal, status, meta_key=terminal%meta_key)
                terminal%modes_saved = status%ok()
            end if

            call terminal_set_terminal_modes(terminal)
        end associate

        if (redraw_after_suspend) selected_frame%needs_redraw = .true.
    end subroutine initialize_all_system_modes

    !> Compute the cursor-motion costs; until then cursor_to does nothing.
    !> Port of the terminal part of term.c:calculate_costs (the
    !> line and character insertion cost tables come with redisplay).
    subroutine calculate_costs(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        terminal%costs_set = .true.
        call calculate_ins_delete_character_costs(terminal)
        call cmcostinit(terminal)
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
    subroutine calculate_ins_delete_character_costs(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer :: ins_startup_cost, delete_startup_cost, ins_cost_per_character, &
                   delete_cost_per_character, columns, i

        if (allocated(terminal%ts_ins_multi_characters)) then
            ins_cost_per_character = 0
            ins_startup_cost = string_cost(terminal%ts_ins_multi_characters)
        else if (allocated(terminal%ts_ins_character) .or. allocated(terminal%ts_pad_inserted_character) .or. &
                 (allocated(terminal%ts_insert_mode) .and. allocated(terminal%ts_end_insert_mode))) then
            ins_startup_cost = (30*(string_cost(terminal%ts_insert_mode) + &
                                    string_cost(terminal%ts_end_insert_mode)))/100
            ins_cost_per_character = string_cost(terminal%ts_ins_character) + string_cost(terminal%ts_pad_inserted_character)
        else
            ins_startup_cost = 9999
            ins_cost_per_character = 0
        end if

        if (allocated(terminal%ts_delete_multi_characters)) then
            delete_cost_per_character = 0
            delete_startup_cost = string_cost(terminal%ts_delete_multi_characters)
        else if (allocated(terminal%ts_delete_character)) then
            delete_startup_cost = string_cost(terminal%ts_delete_mode) + string_cost(terminal%ts_end_delete_mode)
            if (terminal%delete_in_insert_mode) delete_startup_cost = delete_startup_cost/2
            delete_cost_per_character = string_cost(terminal%ts_delete_character)
        else
            delete_startup_cost = 9999
            delete_cost_per_character = 0
        end if

        columns = terminal%wcm%cursor_motion_columns
        if (allocated(terminal%character_ins_delete_cost)) deallocate (terminal%character_ins_delete_cost)
        allocate (terminal%character_ins_delete_cost(-columns:columns))

        do i = -1, -columns, -1
            delete_startup_cost = delete_startup_cost + delete_cost_per_character
            terminal%character_ins_delete_cost(i) = delete_startup_cost
        end do

        terminal%character_ins_delete_cost(0) = 0

        do i = 1, columns
            ins_startup_cost = ins_startup_cost + ins_cost_per_character
            terminal%character_ins_delete_cost(i) = ins_startup_cost
        end do
    end subroutine calculate_ins_delete_character_costs

    !> Map the escape sequences the terminal's function keys send to
    !> function-key events in input-decode-map.  Port of
    !> term.c:term_get_fkeys and term.c:term_get_fkeys_1 (keys that would clash with a non-prefix
    !> binding are skipped, as upstream ignores the error).
    subroutine term_get_fkeys(terminal)
        type(terminal_display_info_type), intent(in) :: terminal
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

        if (.not. is_keymap_live(input_decode_map)) call make_sparse_keymap(input_decode_map)

        do i = 1, NKEYS
            if (caps(i) == "  ") cycle
            seq = tgetstr(terminal%ti, caps(i), found)
            if (found) call define_fkey(seq, trim(names(i)))
        end do

        ! k0 is F0 if k; exists, else F10.
        k_semi = tgetstr(terminal%ti, "k;", found_semi)
        k0 = tgetstr(terminal%ti, "k0", found0)

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

            seq = tgetstr(terminal%ti, trim(fcap), found)

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
                key(k) = value_type(int(iachar(sequence(k:k)), int64))
            end do

            call define_key(input_decode_map, key, list(value_type(name=name)), st)
        end subroutine define_fkey

        subroutine reassign(cap1, cap2, name)
            character(len=*), intent(in) :: cap1, cap2, name
            character(len=:), allocatable :: s
            logical :: has

            s = tgetstr(terminal%ti, cap1, has)
            if (has) return
            s = tgetstr(terminal%ti, cap2, has)
            if (has) call define_fkey(s, name)
        end subroutine reassign
    end subroutine term_get_fkeys

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------

    !> Port of term.c:tty_ring_bell.
    subroutine terminal_ring_bell(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(status_type) :: status

        if (allocated(terminal%ts_visible_bell) .and. visible_bell) then
            call terminal_output(terminal, terminal%ts_visible_bell)
        else
            call terminal_output(terminal, terminal%ts_bell)
        end if

        call terminal_flush(terminal, status)
    end subroutine terminal_ring_bell

    !> Enter full-screen mode.  Port of term.c:tty_set_terminal_modes.
    subroutine terminal_set_terminal_modes(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(status_type) :: status

        if (allocated(terminal%ts_termcap_modes)) then
            call terminal_output(terminal, terminal%ts_termcap_modes)
        else
            ! Push the old contents off the screen.
            call terminal_put_bytes(terminal, repeat(achar(10), terminal%wcm%cursor_motion_rows))
        end if

        if (visible_cursor) then
            call terminal_output(terminal, terminal%ts_cursor_visible)
        else
            call terminal_output(terminal, terminal%ts_cursor_normal)
        end if

        call terminal_output(terminal, terminal%ts_keypad_mode)
        call losecursor(terminal)
        call terminal_flush(terminal, status)
    end subroutine terminal_set_terminal_modes

    !> Leave full-screen mode.  Port of term.c:tty_reset_terminal_modes.
    subroutine terminal_reset_terminal_modes(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(status_type) :: status

        call terminal_turn_off_highlight(terminal)
        call terminal_turn_off_insert(terminal)
        call terminal_output(terminal, terminal%ts_end_keypad_mode)
        call terminal_output(terminal, terminal%ts_cursor_normal)
        call terminal_output(terminal, terminal%ts_end_termcap_modes)
        call terminal_output(terminal, terminal%ts_original_pair)
        call terminal_put_bytes(terminal, achar(13))
        call terminal_flush(terminal, status)
    end subroutine terminal_reset_terminal_modes

    !> Port of term.c:tty_update_end.
    subroutine terminal_update_end(terminal, cursor_off)
        type(terminal_display_info_type), intent(inout) :: terminal
        logical, intent(in) :: cursor_off
        type(status_type) :: status

        if (.not. cursor_off) call terminal_show_cursor(terminal)
        call terminal_turn_off_insert(terminal)
        call terminal_background_highlight(terminal)
        call terminal_flush(terminal, status)
    end subroutine terminal_update_end

    !> Limit scrolling to the first `size' lines (0: all).  Port of
    !> term.c:tty_set_terminal_window.
    subroutine terminal_set_terminal_window(terminal, size)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: size

        terminal%specified_window = size
        if (size == 0) terminal%specified_window = terminal%wcm%cursor_motion_rows
        if (terminal%scroll_region_ok) call terminal_set_scroll_region(terminal, 0, terminal%specified_window)
    end subroutine terminal_set_terminal_window

    !> Port of term.c:tty_set_scroll_region (lines `start'..`stop'-1).
    subroutine terminal_set_scroll_region(terminal, start, stop)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: start, stop
        character(len=:), allocatable :: buffer
        integer(int64) :: rows

        rows = terminal%wcm%cursor_motion_rows

        if (allocated(terminal%ts_set_scroll_region)) then
            buffer = tparam(terminal%ts_set_scroll_region, int(start, int64), int(stop - 1, int64), &
                            0_int64, 0_int64)
        else if (allocated(terminal%ts_set_scroll_region_1)) then
            buffer = tparam(terminal%ts_set_scroll_region_1, rows, int(start, int64), rows - stop, rows)
        else if (allocated(terminal%ts_set_window)) then
            buffer = tparam(terminal%ts_set_window, int(start, int64), 0_int64, int(stop, int64), &
                            int(terminal%wcm%cursor_motion_columns, int64))
        else
            return
        end if

        call terminal_output(terminal, buffer)
        call losecursor(terminal)
    end subroutine terminal_set_scroll_region

    !> Port of term.c:tty_turn_on_insert.
    subroutine terminal_turn_on_insert(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (.not. terminal%insert_mode) call terminal_output(terminal, terminal%ts_insert_mode)
        terminal%insert_mode = .true.
    end subroutine terminal_turn_on_insert

    !> Port of term.c:tty_turn_off_insert.
    subroutine terminal_turn_off_insert(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (terminal%insert_mode) call terminal_output(terminal, terminal%ts_end_insert_mode)
        terminal%insert_mode = .false.
    end subroutine terminal_turn_off_insert

    !> Port of term.c:tty_turn_off_highlight.
    subroutine terminal_turn_off_highlight(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (terminal%standout_mode) call terminal_output(terminal, terminal%ts_end_standout_mode)
        terminal%standout_mode = .false.
    end subroutine terminal_turn_off_highlight

    !> Port of term.c:tty_turn_on_highlight.
    subroutine terminal_turn_on_highlight(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (.not. terminal%standout_mode) call terminal_output(terminal, terminal%ts_standout_mode)
        terminal%standout_mode = .true.
    end subroutine terminal_turn_on_highlight

    !> Port of term.c:tty_toggle_highlight.
    subroutine terminal_toggle_highlight(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (terminal%standout_mode) then
            call terminal_turn_off_highlight(terminal)
        else
            call terminal_turn_on_highlight(terminal)
        end if
    end subroutine terminal_toggle_highlight

    !> Port of term.c:tty_hide_cursor.
    subroutine terminal_hide_cursor(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (terminal%cursor_hidden) return
        terminal%cursor_hidden = .true.
        call terminal_output(terminal, terminal%ts_cursor_invisible)
    end subroutine terminal_hide_cursor

    !> Port of term.c:tty_show_cursor.
    subroutine terminal_show_cursor(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (.not. terminal%cursor_hidden) return
        terminal%cursor_hidden = .false.
        call terminal_output(terminal, terminal%ts_cursor_normal)
        if (visible_cursor) call terminal_output(terminal, terminal%ts_cursor_visible)
    end subroutine terminal_show_cursor

    !> Port of term.c:tty_background_highlight (also
    !> terminal_highlight_if_desired, which is identical).
    subroutine terminal_background_highlight(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (inverse_video) then
            call terminal_turn_on_highlight(terminal)
        else
            call terminal_turn_off_highlight(terminal)
        end if
    end subroutine terminal_background_highlight

    ! ------------------------------------------------------------------
    ! Cursor motion and clearing
    ! ------------------------------------------------------------------

    !> Move the cursor to row `vertical_position', column `horizontal_position' (0-based).  Port of
    !> term.c:tty_cursor_to.
    subroutine terminal_cursor_to(terminal, vertical_position, horizontal_position)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: vertical_position, horizontal_position

        if (.not. terminal%costs_set) return

        if (terminal%wcm%cursor_motion_cury == vertical_position .and. terminal%wcm%cursor_motion_curx == horizontal_position) then
            return
        end if

        if (.not. terminal%tf_standout_motion) call terminal_background_highlight(terminal)
        if (.not. terminal%tf_insmode_motion) call terminal_turn_off_insert(terminal)
        call cmgoto(terminal, vertical_position, horizontal_position)
    end subroutine terminal_cursor_to

    !> Like terminal_cursor_to, in absolute terminal coordinates.  Port of
    !> term.c:tty_raw_cursor_to.
    subroutine terminal_raw_cursor_to(terminal, row, column)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: row, column

        if (terminal%wcm%cursor_motion_cury == row .and. terminal%wcm%cursor_motion_curx == column) return
        if (.not. terminal%tf_standout_motion) call terminal_background_highlight(terminal)
        if (.not. terminal%tf_insmode_motion) call terminal_turn_off_insert(terminal)
        call cmgoto(terminal, row, column)
    end subroutine terminal_raw_cursor_to

    !> Clear from the cursor to the end of the screen.  Port of
    !> term.c:tty_clear_to_end.
    subroutine terminal_clear_to_end(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer :: i

        if (allocated(terminal%ts_clr_to_bottom)) then
            call terminal_background_highlight(terminal)
            call terminal_output(terminal, terminal%ts_clr_to_bottom)
        else
            do i = terminal%wcm%cursor_motion_cury, terminal%wcm%cursor_motion_rows - 1
                call terminal_cursor_to(terminal, i, 0)
                call terminal_clear_end_of_line(terminal, terminal%wcm%cursor_motion_columns)
            end do
        end if
    end subroutine terminal_clear_to_end

    !> Clear the whole screen.  Port of term.c:tty_clear_frame.
    subroutine terminal_clear_frame(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (allocated(terminal%ts_clr_frame)) then
            call terminal_background_highlight(terminal)
            call terminal_output(terminal, terminal%ts_clr_frame)
            call cmat(terminal, 0, 0)
        else
            call terminal_cursor_to(terminal, 0, 0)
            call terminal_clear_to_end(terminal)
        end if
    end subroutine terminal_clear_frame

    !> Clear from the cursor to column `first_unused_horizontal_position' (the end of
    !> what was written).  Port of term.c:tty_clear_end_of_line.
    subroutine terminal_clear_end_of_line(terminal, first_unused_horizontal_position)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: first_unused_horizontal_position
        integer :: last

        if (.not. terminal%costs_set) return
        if (terminal%wcm%cursor_motion_curx >= first_unused_horizontal_position) return
        call terminal_background_highlight(terminal)

        if (allocated(terminal%ts_clr_line)) then
            call terminal_output(terminal, terminal%ts_clr_line)
        else
            ! No kill-line: write spaces, but never in the last cell of an
            ! autowrapping terminal.
            call terminal_turn_off_insert(terminal)
            last = first_unused_horizontal_position
            if (terminal%wcm%cursor_motion_autowrap .and. terminal%wcm%cursor_motion_cury == terminal%wcm%cursor_motion_rows - 1 &
                .and. last == terminal%wcm%cursor_motion_columns) last = last - 1

            if (last > terminal%wcm%cursor_motion_curx) then
                call terminal_put_bytes(terminal, repeat(" ", last - terminal%wcm%cursor_motion_curx))
                call cmplus(terminal, last - terminal%wcm%cursor_motion_curx)
            end if
        end if
    end subroutine terminal_clear_end_of_line

    ! ------------------------------------------------------------------
    ! Glyphs
    ! ------------------------------------------------------------------

    !> The bytes that show `glyphs' on the terminal, encoded with the
    !> terminal coding system (`set_terminal_coding_system'); padding
    !> glyphs (the rest of a wide character) produce nothing.  Port of
    !> term.c:encode_terminal_code (no composition, display table glyph
    !> strings or terminal-specific charset lists).
    function encode_terminal_code(glyphs) result(bytes)
        type(glyph_type), intent(in) :: glyphs(:)
        character(len=:), allocatable :: bytes
        character(len=MAXIMUM_MULTIBYTE_LENGTH) :: buffer
        character(len=:), allocatable :: coding, text
        integer(int32) :: n
        logical :: unencodable
        type(status_type) :: status
        integer :: k

        bytes = ""
        coding = terminal_coding_system()

        if (len(coding) > 0) then
            if (coding_system_type(coding) /= "utf-8") then
                ! The text in the internal representation, encoded.
                text = ""
                unencodable = .false.

                do k = 1, size(glyphs)
                    if (glyphs(k)%padding) then
                        ! The rest of a wide character it can't encode.
                        if (unencodable) text = text//"?"
                        cycle
                    end if

                    unencodable = .not. is_character_encodable(int(glyphs(k)%ch), coding)

                    if (unencodable) then
                        ! C is not encodable.
                        text = text//"?"
                    else
                        call character_string(glyphs(k)%ch, buffer, n, status)
                        if (status%ok()) text = text//buffer(1:n)
                    end if
                end do

                call encode_coding_string(text, coding, bytes, status)
                if (.not. status%ok()) bytes = text
                return
            end if
        end if

        ! UTF-8, or no terminal coding system: the internal representation.
        do k = 1, size(glyphs)
            if (glyphs(k)%padding) cycle

            if (is_character_byte8(glyphs(k)%ch)) then
                bytes = bytes//char(character_to_byte8(glyphs(k)%ch))
            else
                call character_string(glyphs(k)%ch, buffer, n, status)

                if (status%ok()) then
                    bytes = bytes//buffer(1:n)
                else
                    bytes = bytes//"?"
                end if
            end if
        end do
    end function encode_terminal_code

    !> Whether attribute `attribute' may be combined with colors.
    !> Port of term.c:MAY_USE_WITH_COLORS_P.
    logical function may_use_with_colors(terminal, attribute)
        type(terminal_display_info_type), intent(in) :: terminal
        integer, intent(in) :: attribute

        may_use_with_colors = .true.
        if (terminal%tn_maximum_colors > 0) may_use_with_colors = iand(terminal%tn_no_color_video, attribute) == 0
    end function may_use_with_colors

    !> Return true if the terminal TERMINAL is capable of displaying all the
    !> attributes in CAPS (TERMINAL_CAP_* bits), else false.  Port of
    !> term.c:tty_capable_p.
    logical function is_terminal_capable(terminal, caps) result(capable)
        type(terminal_display_info_type), intent(in) :: terminal
        integer, intent(in) :: caps

        capable = .false.
        if (.not. try(TERMINAL_CAP_INVERSE, allocated(terminal%ts_standout_mode), NC_REVERSE)) return
        if (.not. try(TERMINAL_CAP_UNDERLINE, allocated(terminal%ts_enter_underline_mode), NC_UNDERLINE)) return
        if (.not. try(TERMINAL_CAP_UNDERLINE_STYLED, allocated(terminal%tf_set_underline_style), NC_UNDERLINE)) return
        if (.not. try(TERMINAL_CAP_BOLD, allocated(terminal%ts_enter_bold_mode), NC_BOLD)) return
        if (.not. try(TERMINAL_CAP_DIM, allocated(terminal%ts_enter_dim_mode), NC_DIM)) return
        if (.not. try(TERMINAL_CAP_ITALIC, allocated(terminal%ts_enter_italic_mode), NC_ITALIC)) return
        if (.not. try(TERMINAL_CAP_STRIKE_THROUGH, allocated(terminal%ts_enter_strike_through_mode), NC_STRIKE_THROUGH)) return
        ! We can do it!
        capable = .true.

    contains

        !> TERMINAL_CAPABLE_P_TRY: false if CAP is asked for and the terminal
        !> lacks it (HAVE) or can't use it with colors.
        logical function try(cap, have, nc_bit)
            integer, intent(in) :: cap, nc_bit
            logical, intent(in) :: have

            try = .true.
            if (iand(caps, cap) /= 0) try = have .and. may_use_with_colors(terminal, nc_bit)
        end function try
    end function is_terminal_capable

    !> Output a color capability for `color'.  Not a port.
    subroutine set_color(terminal, cap, color)
        type(terminal_display_info_type), intent(inout) :: terminal
        character(len=:), allocatable, intent(in) :: cap
        integer(int64), intent(in) :: color
        character(len=:), allocatable :: p

        if (.not. allocated(cap)) return

        if (terminal%tf_rgb_separate) then
            p = tparam(cap, ishft(color, -16), iand(ishft(color, -8), 255_int64), &
                       iand(color, 255_int64), 0_int64)
        else
            p = tparam(cap, color, 0_int64, 0_int64, 0_int64)
        end if

        call terminal_output(terminal, p)
    end subroutine set_color

    !> Start drawing in `face'.  Port of term.c:turn_on_face.
    subroutine turn_on_face(terminal, face)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(face_type), intent(in) :: face
        integer(int64) :: foreground, background
        character(len=:), allocatable :: p

        foreground = face%foreground
        background = face%background

        if (may_use_with_colors(terminal, NC_REVERSE)) then
            if (inverse_video) then
                if (foreground == FACE_TERMINAL_DEFAULT_FOREGROUND_COLOR .or. background &
                    == FACE_TERMINAL_DEFAULT_BACKGROUND_COLOR) then
                    call terminal_toggle_highlight(terminal)
                end if
            else
                if (foreground == FACE_TERMINAL_DEFAULT_BACKGROUND_COLOR .or. background &
                    == FACE_TERMINAL_DEFAULT_FOREGROUND_COLOR) then
                    call terminal_toggle_highlight(terminal)
                end if
            end if
        end if

        if (face%terminal_bold_p .and. may_use_with_colors(terminal, NC_BOLD)) then
            call terminal_output(terminal, terminal%ts_enter_bold_mode)
        end if

        if (face%terminal_italic_p .and. may_use_with_colors(terminal, NC_ITALIC)) then
            if (allocated(terminal%ts_enter_italic_mode)) then
                call terminal_output(terminal, terminal%ts_enter_italic_mode)
            else
                call terminal_output(terminal, terminal%ts_enter_dim_mode)
            end if
        end if

        if (face%underline /= FACE_NO_UNDERLINE .and. may_use_with_colors(terminal, NC_UNDERLINE)) then
            if (face%underline == FACE_UNDERLINE_SINGLE .or. &
                .not. allocated(terminal%tf_set_underline_style)) then
                call terminal_output(terminal, terminal%ts_enter_underline_mode)
            else
                p = tparam(terminal%tf_set_underline_style, int(face%underline, int64), 0_int64, &
                           0_int64, 0_int64)
                call terminal_output(terminal, p)
            end if
        end if

        if (face%terminal_strike_through_p .and. may_use_with_colors(terminal, NC_STRIKE_THROUGH)) then
            call terminal_output(terminal, terminal%ts_enter_strike_through_mode)
        end if

        if (terminal%tn_maximum_colors > 0) then
            ! In standout mode foreground and background are swapped.
            if (face_terminal_specified_color(foreground)) then
                if (terminal%standout_mode) then
                    call set_color(terminal, terminal%ts_set_background, foreground)
                else
                    call set_color(terminal, terminal%ts_set_foreground, foreground)
                end if
            end if

            if (face_terminal_specified_color(background)) then
                if (terminal%standout_mode) then
                    call set_color(terminal, terminal%ts_set_foreground, background)
                else
                    call set_color(terminal, terminal%ts_set_background, background)
                end if
            end if

            if (allocated(terminal%tf_set_underline_color) .and. face%underline_color /= 0) then
                p = tparam(terminal%tf_set_underline_color, face%underline_color, 0_int64, 0_int64, 0_int64)
                call terminal_output(terminal, p)
            end if
        end if
    end subroutine turn_on_face

    !> Stop drawing in `face'.  Port of term.c:turn_off_face.
    subroutine turn_off_face(terminal, face)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(face_type), intent(in) :: face

        if (allocated(terminal%ts_exit_attribute_mode)) then
            if (face%terminal_bold_p .or. face%terminal_italic_p .or. face%terminal_reverse_p .or. &
                face%underline /= FACE_NO_UNDERLINE .or. face%terminal_strike_through_p) then
                call terminal_output(terminal, terminal%ts_exit_attribute_mode)

                if (allocated(terminal%ts_end_standout_mode)) then
                    if (terminal%ts_exit_attribute_mode == terminal%ts_end_standout_mode) then
                        terminal%standout_mode = .false.
                    end if
                end if
            end if
        else if (face%underline /= FACE_NO_UNDERLINE) then
            call terminal_output(terminal, terminal%ts_exit_underline_mode)
        end if

        if (terminal%tn_maximum_colors > 0) then
            if ((face%foreground /= FACE_TERMINAL_DEFAULT_COLOR .and. &
                 face%foreground /= FACE_TERMINAL_DEFAULT_FOREGROUND_COLOR) .or. &
                (face%background /= FACE_TERMINAL_DEFAULT_COLOR .and. &
                 face%background /= FACE_TERMINAL_DEFAULT_BACKGROUND_COLOR)) then
                call terminal_output(terminal, terminal%ts_original_pair)
            end if
        end if
    end subroutine turn_off_face

    !> The face for `face_id', or the default face if out of range.
    !> Not a port.
    function face_of(faces, face_id) result(face)
        type(face_type), intent(in) :: faces(0:)
        integer(int32), intent(in) :: face_id
        type(face_type) :: face

        if (face_id >= 0 .and. face_id <= ubound(faces, 1)) face = faces(face_id)
    end function face_of

    !> Write `glyphs' at the cursor, in runs of the same face.  Port of
    !> term.c:tty_write_glyphs_1.
    subroutine terminal_write_glyphs_1(terminal, glyphs, faces)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(glyph_type), intent(in) :: glyphs(:)
        type(face_type), intent(in) :: faces(0:)
        type(face_type) :: face
        integer :: start, n, length_

        call terminal_turn_off_insert(terminal)
        call terminal_hide_cursor(terminal)
        length_ = size(glyphs)
        if (length_ == 0) return
        call cmplus(terminal, length_)
        start = 1

        do while (start <= length_)
            n = 1

            do while (start + n <= length_)
                if (glyphs(start + n)%face_id /= glyphs(start)%face_id) exit
                n = n + 1
            end do

            call terminal_background_highlight(terminal)
            face = face_of(faces, glyphs(start)%face_id)
            call turn_on_face(terminal, face)
            call terminal_put_bytes(terminal, encode_terminal_code(glyphs(start:start + n - 1)))
            call turn_off_face(terminal, face)
            call terminal_turn_off_highlight(terminal)
            start = start + n
        end do

        call cmcheckmagic(terminal)
    end subroutine terminal_write_glyphs_1

    !> Write `glyphs' at the cursor.  Writing the last cell of an
    !> autowrapping terminal would scroll it, so that cell is inserted
    !> instead; when only that cell is written, `previous' (the glyph before
    !> it, which upstream reads from the glyph row) is rewritten too, and
    !> without it the last cell is left alone.  Port of
    !> term.c:tty_write_glyphs.
    recursive subroutine terminal_write_glyphs(terminal, glyphs, faces, previous)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(glyph_type), intent(in) :: glyphs(:)
        type(face_type), intent(in) :: faces(0:)
        type(glyph_type), intent(in), optional :: previous
        type(glyph_type), allocatable :: run(:)
        integer :: length_, old_x, old_y

        length_ = size(glyphs)

        if (terminal%wcm%cursor_motion_autowrap .and. terminal%wcm%cursor_motion_cury + 1 == terminal%wcm%cursor_motion_rows .and. &
            terminal%wcm%cursor_motion_curx + length_ == terminal%wcm%cursor_motion_columns .and. length_ > 0) then
            if (length_ == 1) then
                if (.not. present(previous) .or. terminal%wcm%cursor_motion_curx == 0) return
                call cmgoto(terminal, terminal%wcm%cursor_motion_cury, terminal%wcm%cursor_motion_curx - 1)
                allocate (run(2))
                run(1) = previous
                run(2) = glyphs(1)
            else
                run = glyphs
            end if

            old_x = terminal%wcm%cursor_motion_curx
            old_y = terminal%wcm%cursor_motion_cury
            call terminal_write_glyphs_1(terminal, run(2:), faces)
            call cmgoto(terminal, old_y, old_x)
            call terminal_insert_glyphs(terminal, faces, run(1:1))
        else
            call terminal_write_glyphs_1(terminal, glyphs, faces)
        end if
    end subroutine terminal_write_glyphs

    !> Insert `len' blanks, or `glyphs' when given, at the cursor.  Port of
    !> term.c:tty_insert_glyphs.
    recursive subroutine terminal_insert_glyphs(terminal, faces, glyphs, len)
        type(terminal_display_info_type), intent(inout) :: terminal
        type(face_type), intent(in) :: faces(0:)
        type(glyph_type), intent(in), optional :: glyphs(:)
        integer, intent(in), optional :: len
        type(face_type) :: face
        character(len=:), allocatable :: buffer
        integer :: n, k

        if (present(glyphs)) then
            n = size(glyphs)
        else
            n = 0
            if (present(len)) n = len
        end if

        if (n <= 0) return

        if (allocated(terminal%ts_ins_multi_characters)) then
            buffer = tparam(terminal%ts_ins_multi_characters, int(n, int64), 0_int64, 0_int64, 0_int64)
            call terminal_output(terminal, buffer)
            if (present(glyphs)) call terminal_write_glyphs(terminal, glyphs, faces)
            return
        end if

        call terminal_turn_on_insert(terminal)
        call cmplus(terminal, n)
        k = 1

        do while (k <= n)
            call terminal_output(terminal, terminal%ts_ins_character)

            if (.not. present(glyphs)) then
                call terminal_put_bytes(terminal, " ")
                call terminal_output(terminal, terminal%ts_pad_inserted_character)
                k = k + 1
                cycle
            end if

            call terminal_background_highlight(terminal)
            face = face_of(faces, glyphs(k)%face_id)
            call turn_on_face(terminal, face)
            call terminal_put_bytes(terminal, encode_terminal_code(glyphs(k:k)))
            k = k + 1

            ! The padding cells of a wide character need room too.
            do while (k <= n)
                if (.not. glyphs(k)%padding) exit
                call terminal_output(terminal, terminal%ts_ins_character)
                k = k + 1
            end do

            call terminal_output(terminal, terminal%ts_pad_inserted_character)
            call turn_off_face(terminal, face)
            call terminal_turn_off_highlight(terminal)
        end do

        call cmcheckmagic(terminal)
    end subroutine terminal_insert_glyphs

    !> Delete `n' characters at the cursor.  Port of term.c:tty_delete_glyphs.
    subroutine terminal_delete_glyphs(terminal, n)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: n
        character(len=:), allocatable :: buffer
        integer :: i

        if (terminal%delete_in_insert_mode) then
            call terminal_turn_on_insert(terminal)
        else
            call terminal_turn_off_insert(terminal)
            call terminal_output(terminal, terminal%ts_delete_mode)
        end if

        if (allocated(terminal%ts_delete_multi_characters)) then
            buffer = tparam(terminal%ts_delete_multi_characters, int(n, int64), 0_int64, 0_int64, 0_int64)
            call terminal_output(terminal, buffer)
        else
            do i = 1, n
                call terminal_output(terminal, terminal%ts_delete_character)
            end do
        end if

        if (.not. terminal%delete_in_insert_mode) call terminal_output(terminal, terminal%ts_end_delete_mode)
    end subroutine terminal_delete_glyphs

    !> Insert `n' blank lines at row `vertical_position' (delete -`n' lines if
    !> negative), within the scroll window.  Port of term.c:tty_ins_del_lines.
    subroutine terminal_ins_delete_lines(terminal, vertical_position, n)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: vertical_position, n
        character(len=:), allocatable :: multi, single, scroll, buffer
        integer :: i, k

        if (n > 0) then
            if (allocated(terminal%ts_ins_multi_lines)) multi = terminal%ts_ins_multi_lines
            if (allocated(terminal%ts_ins_line)) single = terminal%ts_ins_line
            if (allocated(terminal%ts_revision_scroll)) scroll = terminal%ts_revision_scroll
        else
            if (allocated(terminal%ts_delete_multi_lines)) multi = terminal%ts_delete_multi_lines
            if (allocated(terminal%ts_delete_line)) single = terminal%ts_delete_line
            if (allocated(terminal%ts_fwd_scroll)) scroll = terminal%ts_fwd_scroll
        end if

        i = abs(n)
        if (terminal%scroll_region_ok .and. vertical_position + i >= terminal%specified_window) return
        if (.not. terminal%memory_below_frame .and. vertical_position + i >= terminal%wcm%cursor_motion_rows) return

        if (allocated(multi)) then
            call terminal_raw_cursor_to(terminal, vertical_position, 0)
            call terminal_background_highlight(terminal)
            buffer = tparam(multi, int(i, int64), 0_int64, 0_int64, 0_int64)
            call terminal_output(terminal, buffer)
        else if (allocated(single)) then
            call terminal_raw_cursor_to(terminal, vertical_position, 0)
            call terminal_background_highlight(terminal)

            do k = 1, i
                call terminal_output(terminal, single)
            end do

            if (terminal%tf_teleray) terminal%wcm%cursor_motion_curx = 0
        else
            call terminal_set_scroll_region(terminal, vertical_position, terminal%specified_window)

            if (n < 0) then
                call terminal_raw_cursor_to(terminal, terminal%specified_window - 1, 0)
            else
                call terminal_raw_cursor_to(terminal, vertical_position, 0)
            end if

            call terminal_background_highlight(terminal)

            do k = 1, i
                call terminal_output(terminal, scroll)
            end do

            call terminal_set_scroll_region(terminal, 0, terminal%specified_window)
        end if

        if (.not. terminal%scroll_region_ok .and. terminal%memory_below_frame .and. n < 0) then
            call terminal_cursor_to(terminal, terminal%wcm%cursor_motion_rows + n, 0)
            call terminal_clear_to_end(terminal)
        end if
    end subroutine terminal_ins_delete_lines

    ! ------------------------------------------------------------------
    ! Lisp-visible functions on a tty (TERMINAL is its display info)
    ! ------------------------------------------------------------------

    !> Return true if the tty device TERMINAL can display colors.
    !> Port of term.c:Ftty_display_color_p.
    logical function is_terminal_display_color(terminal)
        type(terminal_display_info_type), intent(in) :: terminal

        is_terminal_display_color = terminal%tn_maximum_colors > 0
    end function is_terminal_display_color

    !> Return the number of colors supported by the tty device TERMINAL.
    !> Port of term.c:Ftty_display_color_cells.
    integer function terminal_display_color_cells(terminal)
        type(terminal_display_info_type), intent(in) :: terminal

        terminal_display_color_cells = terminal%tn_maximum_colors
    end function terminal_display_color_cells

    !> Return the type of the tty device that TERMINAL uses (the empty
    !> string if it is not on a tty device).  Port of term.c:Ftty_type
    !> (with term.c:tty_type_name).
    function terminal_type(terminal) result(name)
        type(terminal_display_info_type), intent(in) :: terminal
        character(len=:), allocatable :: name

        name = ""
        if (allocated(terminal%type)) name = terminal%type
    end function terminal_type

    !> Return true if TERMINAL is the controlling tty of the Fortmacs process.
    !> Port of term.c:Fcontrolling_tty_p.
    logical function is_controlling_terminal(terminal)
        type(terminal_display_info_type), intent(in) :: terminal

        is_controlling_terminal = .false.
        if (allocated(terminal%name)) is_controlling_terminal = terminal%name == "/dev/tty"
    end function is_controlling_terminal

    !> Declare that the tty used by TERMINAL does not handle underlining.
    !> This is used to override the terminfo data, for certain terminals that
    !> do not really do underlining, but say that they do.  Port of
    !> term.c:Ftty_no_underline.
    subroutine terminal_no_underline(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (allocated(terminal%ts_enter_underline_mode)) deallocate (terminal%ts_enter_underline_mode)
    end subroutine terminal_no_underline
end module terminal_output_mod
