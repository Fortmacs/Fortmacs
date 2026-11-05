! frame_mod.f90 -- frames: a terminal screen with its glyph matrices.
! Ported from GNU Emacs 31.1 src/frame.h (struct frame, the parts in use),
! src/frame.c (the divider widths) and lisp/frame.el (window dividers, in
! the submodule frame_cmds_mod).
!
! Copyright (C) 1993-1994, 1999-2026 Free Software Foundation, Inc.
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

!> One frame per terminal, so a frame holds its terminal directly.  The
!> current matrix is what the screen shows; redisplay fills the desired
!> matrix, and update_frame (dispnew_mod) makes the screen match it.
!> The frame has a window tree (its root window), the selected window (a
!> live window of the tree) and a minibuffer window on its last line, all
!> windows of window_mod's registry; child frames are not ported.
!>
!> Sizes are in pixels where upstream has them: FRAME_COLUMN_WIDTH and
!> FRAME_LINE_HEIGHT are 1 on a terminal frame, the default font's width
!> and height on a window-system frame (one with a redisplay interface,
!> `rif', and a font driver).  Windows are sized in columns and lines
!> (`frame_resize_pixelwise' and `window_resize_pixelwise' .false.), so their
!> pixel edges are those times the frame's column width and line height.
!> Window-system frames exist only in the tests (a fake device) until the
!> GUI is ported.
!>
!> Frame parameters are typed components (`frame_parameters_type'): a
!> frame's own in `parameters', those for new frames in `default_frame'.
!> A terminal frame stores them but has no window dividers, so its divider
!> widths stay 0, as upstream's.
module frame_mod
    use termchar_mod, only: tty_display_info_type
    use dispextern_mod, only: glyph_matrix_type, face_type, redisplay_interface_type
    use font_mod, only: font_driver_type
    use handle_mod, only: handle_type, slot_table_type, null_handle
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use hooks_mod, only: hook_type
    use, intrinsic :: iso_fortran_env, only: int64
    use window_mod, only: window_type, window_ref, is_window_valid, make_window, windows, window_text_cols, &
                          window_safe_min_height, window_safe_min_width, window_resize_root_window, &
                          window_resize_check, window_resize_apply, same_handle, &
                          window_configuration_type, destroy_window_configuration
    use marker_mod, only: destroy_marker
    use timefns_mod, only: time_type
    implicit none(type, external)
    private

    !> A window configuration kept with the point of its current buffer
    !> (tab-bar.el's `wc' and `wc-point' entries of a tab or of a tab's
    !> history).  Free it with its `destroy'.  Not a port.
    type, public :: tab_window_configuration_type
        type(window_configuration_type) :: window_configuration
        !> A marker at point in the buffer that was current (null for
        !> none).
        type(handle_type) :: point
    contains
        !> Free the configuration and the marker.
        procedure :: destroy => destroy_tab_window_configuration
    end type tab_window_configuration_type

    !> A tab of a frame's tab bar (an element of the frame parameter
    !> `tabs', as tab-bar.el's tab-bar--tab and tab-bar--current-tab-make
    !> make it): its name, whether the user gave it, its group, when it
    !> was last current and, for a tab that is not current, the window
    !> configuration and buffer lists it restores when selected.  Free it
    !> with its `destroy'.  Not a port (the window state `ws' is Deferred:
    !> docs/status/tab_bar_mod.md).
    type, public :: tab_type
        !> The frame's current tab (`current-tab'; else `tab').
        logical :: current = .false.
        character(len=:), allocatable :: name
        !> The name was given by the user (`explicit-name').
        logical :: explicit_name = .false.
        !> The tab's group (`group'); unallocated for none.
        character(len=:), allocatable :: group
        !> When the tab stopped being current (`time').
        type(time_type) :: time
        !> A tab that is not current: the window configuration with its
        !> point (`wc', `wc-point'), and the frame's buffer lists then
        !> (`wc-bl', `wc-bbl').
        logical :: has_window_configuration = .false.
        type(tab_window_configuration_type) :: window_configuration
        type(handle_type), allocatable :: buffers(:), buried_buffers(:)
        !> The frame's tab history then, while `tab_bar_history_mode' was
        !> on (`wc-history-back', `wc-history-forward').
        logical :: has_history = .false.
        type(tab_window_configuration_type), allocatable :: history_back(:), history_forward(:)
    contains
        !> Free the tab's window configurations and markers.
        procedure :: destroy => destroy_tab
    end type tab_type

    !> The frame parameters ported (frame.h param_alist; frame.c's
    !> frame_parms), as typed components.
    type, public :: frame_parameters_type
        !> Width (in pixels) of the dividers on the right of windows
        !> (`right-divider-width'); 0 for none.
        integer :: right_divider_width = 0
        !> Width (in pixels) of the dividers on the bottom of windows
        !> (`bottom-divider-width'); 0 for none.
        integer :: bottom_divider_width = 0
        !> The number of tab bar lines of new frames (`tab-bar-lines' in
        !> `default_frame'; a frame's own is frame_type's `tab_bar_lines').
        integer :: tab_bar_lines = 0
        !> `toggle_frame_tab_bar' set the tab bar lines, which
        !> `tab_bar_mode' then keeps (`tab-bar-lines-keep-state').
        logical :: tab_bar_lines_keep_state = .false.
        !> The frame's tabs (`tabs'); unallocated before the tab bar
        !> made them.
        type(tab_type), allocatable :: tabs(:)
    end type frame_parameters_type

    !> Default values of frame parameters for frame creation.
    !> Note that many display-related modes (like `window_divider_mode')
    !> alter `default_frame', so if you set this variable directly, you
    !> may be overriding other settings unintentionally.  Port of frame.c
    !> default-frame-alist (the parameters ported).
    type(frame_parameters_type), public :: default_frame

    !> Values of `window_divider_default_places': dividers on the bottom of
    !> each window only, on the right only, or on both.
    integer, parameter, public :: WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY = 1, &
                                  WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY = 2, &
                                  WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT = 3

    !> Default positions of window dividers.
    !> Possible values are `WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY'
    !> (dividers on the bottom of each window only),
    !> `WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY' (dividers on the right of
    !> each window only), and `WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT'
    !> (dividers on the bottom and on the right of each window).  The
    !> default is `WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY'.
    !>
    !> The value takes effect if and only if dividers are enabled by
    !> `window_divider_mode'.
    !>
    !> To position dividers on frames individually, use the frame
    !> parameters `bottom_divider_width' and `right_divider_width'.
    !> Port of frame.el window-divider-default-places.
    integer, public :: window_divider_default_places = WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY

    !> Default width of dividers on bottom of windows.
    !> The value must be a positive integer and takes effect when bottom
    !> dividers are displayed by `window_divider_mode'.
    !>
    !> To adjust bottom dividers for frames individually, use the frame
    !> parameter `bottom_divider_width'.  Port of frame.el
    !> window-divider-default-bottom-width.
    integer, public :: window_divider_default_bottom_width = 6

    !> Default width of dividers on the right of windows.
    !> The value must be a positive integer and takes effect when right
    !> dividers are displayed by `window_divider_mode'.
    !>
    !> To adjust right dividers for frames individually, use the frame
    !> parameter `right_divider_width'.  Port of frame.el
    !> window-divider-default-right-width.
    integer, public :: window_divider_default_right_width = 6

    !> Hook run after entering or leaving `window_divider_mode'.  Port of
    !> frame.el window-divider-mode-hook.
    type(hook_type), target, public :: window_divider_mode_hook = hook_type(name="window_divider_mode_hook")

    !> Port of the parts of frame.h:struct frame in use.
    type, public :: frame_type
        type(tty_display_info_type) :: tty
        type(glyph_matrix_type) :: current_matrix, desired_matrix
        !> The root of the window tree, the minibuffer window and the
        !> selected window (frame.h root_window, minibuffer_window,
        !> selected_window).
        type(handle_type) :: root_window, minibuffer_window, selected_window
        !> The realized faces, indexed by face id (FRAME_FACE_CACHE).
        type(face_type), allocatable :: faces(:)
        !> Where to leave the cursor after an update (set by redisplay from
        !> the selected window's cursor), 0-based; -1 for none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> The minibuffer window is the selected window (while a minibuffer
        !> is active; `selected_window' is the window it was entered from).
        logical :: minibuffer_selected = .false.
        !> The screen contents are unknown and must be redrawn
        !> (frame.h:garbaged, set by frame.h:SET_FRAME_GARBAGED).
        logical :: needs_redraw = .true.
        logical :: glyphs_initialized_p = .false.
        !> How redisplay draws on a window-system frame (FRAME_RIF); not
        !> associated on a terminal frame.
        class(redisplay_interface_type), pointer :: rif => null()
        !> The fonts of a window-system frame's faces (f->font_driver_list).
        class(font_driver_type), pointer :: font_driver => null()
        !> FRAME_COLUMN_WIDTH and FRAME_LINE_HEIGHT: the canonical
        !> character width and line height in pixels.
        integer :: column_width = 1, line_height = 1
        !> The narrowest glyph and the lowest font of the frame's display
        !> (FRAME_SMALLEST_CHAR_WIDTH, FRAME_SMALLEST_FONT_HEIGHT), which
        !> bound how many glyphs and rows a window can show.
        integer :: smallest_char_width = 1, smallest_font_height = 1
        !> The size of a window-system frame in columns and lines
        !> (text_cols, text_lines); a terminal frame has its terminal's.
        integer :: text_cols = 0, text_lines = 0
        !> A size change requested but not made yet (delayed by
        !> dispnew_mod's `change_frame_size'): the new width and height in
        !> pixels, -1 for no change, and whether one is pending
        !> (frame.h:new_width, frame.h:new_height, frame.h:new_size_p).
        integer :: new_width = -1, new_height = -1
        logical :: new_size_pending = .false.
        !> The frame's parameters (those ported).
        type(frame_parameters_type) :: parameters
        !> The widths of the window dividers drawn, in pixels
        !> (frame.h:right_divider_width, frame.h:bottom_divider_width):
        !> always 0 on a terminal frame, which draws none.
        integer :: right_divider_width = 0, bottom_divider_width = 0
        !> The frame's handle in the frame registry (what upstream's Lisp
        !> object for the frame is): `selected_frame%handle' is
        !> frame.c:Fselected_frame.
        type(handle_type) :: handle
        !> The frame's name (frame.h:name): F<num> on a terminal, unless
        !> the user named it (frame.h:explicit_name).
        character(len=:), allocatable :: name
        logical :: explicit_name = .false.
        !> The frame is visible (frame.h:visible); every terminal frame is.
        logical :: visible = .true.
        !> The frame's buffer lists while another frame is selected
        !> (frame.h buffer_list and buried_buffer_list; buffer_mod holds
        !> the selected frame's).
        type(handle_type), allocatable :: buffers(:), buried_buffers(:)
        !> The number of lines of the frame's tab bar, at its top
        !> (frame.h:tab_bar_lines, the `tab-bar-lines' parameter); set
        !> with `set_tab_bar_lines'.
        integer :: tab_bar_lines = 0
        !> The window configurations of the frame's current tab before the
        !> latest changes, the newest first, and those gone back over
        !> (tab-bar.el's tab-bar-history-back and tab-bar-history-forward
        !> of the frame).
        type(tab_window_configuration_type), allocatable :: tab_history_back(:), tab_history_forward(:)
    end type frame_type

    !> One frame of the registry, made by make_frame and destroyed by
    !> delete_frame; a pointer, so that `selected_frame' and the frames'
    !> handles stay valid while frames are made.  Not a port (upstream
    !> relies on GC).
    type :: frame_pointer_type
        type(frame_type), pointer :: frame => null()
    end type frame_pointer_type

    !> The initial terminal frame, F1 (frame.c:make_initial_frame).
    type(frame_type), target :: initial_frame
    !> The selected frame (frame.c:selected_frame): the frame whose
    !> selected window commands apply to, on a terminal the frame it shows
    !> (termchar.h:top_frame).
    type(frame_type), pointer, public :: selected_frame => initial_frame

    !> The frame registry: slots, their frames, and the live frames in the
    !> order of frame.c:Vframe_list (the newest first).
    type(slot_table_type) :: frame_table
    type(frame_pointer_type), allocatable :: frame_slots(:)
    type(handle_type), allocatable :: frame_order(:)
    !> The number of the last F<num> name given (frame.c:tty_frame_count).
    integer(int64) :: tty_frame_count = 1
    !> The frame the terminal shows (termchar.h:top_frame): the selected
    !> frame, except while `select_frame_begin' has selected another one.
    type(handle_type) :: top_frame

    public :: ensure_frame_windows, selected_window, selected_window_ref, frame_selected_window_ref, minibuffer_window_ref
    public :: frame_lines, frame_cols, window_pixel_left, window_pixel_top, window_pixel_width, &
              window_pixel_height, window_body_pixel_width
    public :: frame_windows_min_size, resize_frame_windows, frame_top_margin, set_tab_bar_lines
    public :: ensure_frames, is_frame_live, get_frame, frame_list, next_frame, previous_frame, find_frame, window_frame, &
              set_term_frame_name, get_next_frame_name, register_frame, unregister_frame
    public :: make_frame, select_frame, select_frame_begin, select_frame_end, delete_frame, delete_other_frames, &
              other_frame, select_frame_by_name, set_frame_name, init_frame_commands
    public :: frame_right_divider_width, frame_bottom_divider_width, window_right_divider_width, &
              window_bottom_divider_width, is_window_divider_width_valid, window_divider_mode, &
              window_divider_mode_apply

    ! frame.el's commands and frame.c's, in submodule frame_cmds_mod (a
    ! minor mode needs easy_mmode_mod, which depends on this module).
    interface
        !> Display dividers between windows (Window Divider mode).
        !>
        !> The option `window_divider_default_places' specifies on which
        !> side of a window dividers are displayed.  The options
        !> `window_divider_default_bottom_width' and
        !> `window_divider_default_right_width' specify their respective
        !> widths.  Port of frame.el:window-divider-mode.
        recursive module subroutine window_divider_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine window_divider_mode

        !> Apply window divider places and widths to all frames.
        !> If ENABLE is .false., apply default places and widths.  Else reset
        !> all divider widths to zero.  Port of
        !> frame.el:window-divider-mode-apply (upstream's docstring has the
        !> two cases the wrong way round: ENABLE applies the defaults).
        module subroutine window_divider_mode_apply(enable)
            implicit none(type, external)
            logical, intent(in) :: enable
        end subroutine window_divider_mode_apply

        !> Make a frame on the selected frame's terminal showing the
        !> current buffer (frame_cmds_mod).  Port of frame.el:make-frame.
        module subroutine make_frame(frame, status)
            implicit none(type, external)
            type(handle_type), intent(out) :: frame
            type(status_type), intent(out) :: status
        end subroutine make_frame

        !> Select FRAME (frame_cmds_mod).  Port of frame.c:Fselect_frame.
        recursive module subroutine select_frame(frame, status, norecord, for_deletion)
            implicit none(type, external)
            type(handle_type), intent(in) :: frame
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord, for_deletion
        end subroutine select_frame

        !> Select FRAME for what follows without showing it, storing the
        !> frame selected in SAVED (frame_cmds_mod).  Port of
        !> subr.el:with-selected-frame.
        module subroutine select_frame_begin(frame, saved)
            implicit none(type, external)
            type(handle_type), intent(in) :: frame
            type(handle_type), intent(out) :: saved
        end subroutine select_frame_begin

        !> Select SAVED again (frame_cmds_mod).  Port of
        !> subr.el:with-selected-frame.
        recursive module subroutine select_frame_end(saved)
            implicit none(type, external)
            type(handle_type), intent(in) :: saved
        end subroutine select_frame_end

        !> Delete FRAME, the selected frame by default (frame_cmds_mod).
        !> Port of frame.c:Fdelete_frame.
        recursive module subroutine delete_frame(status, frame, force)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: frame
            logical, intent(in), optional :: force
        end subroutine delete_frame

        !> Delete all frames but FRAME (frame_cmds_mod).  Port of
        !> frame.el:delete-other-frames.
        recursive module subroutine delete_other_frames(status, frame, iconify)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: frame
            logical, intent(in), optional :: iconify
        end subroutine delete_other_frames

        !> Select the ARGth frame from the selected one (frame_cmds_mod).
        !> Port of frame.el:other-frame.
        recursive module subroutine other_frame(arg, status)
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine other_frame

        !> Select the frame named NAME (frame_cmds_mod).  Port of
        !> frame.el:select-frame-by-name.
        recursive module subroutine select_frame_by_name(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine select_frame_by_name

        !> Name the selected frame NAME (frame_cmds_mod).  Port of
        !> frame.el:set-frame-name.
        module subroutine set_frame_name(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine set_frame_name

        !> Give frame F N tab bar lines,
        !> resizing its windows when the number changes.  Port of
        !> frame.c:set_tab_bar_lines.
        module subroutine set_tab_bar_lines(f, n)
            implicit none(type, external)
            type(frame_type), intent(inout) :: f
            integer, intent(in) :: n
        end subroutine set_tab_bar_lines

        !> Register the frame commands (frame_cmds_mod).  Not a port.
        module subroutine init_frame_commands()
            implicit none(type, external)
        end subroutine init_frame_commands
    end interface

contains

    !> Free window configuration SELF and its marker.  Not a port.
    subroutine destroy_tab_window_configuration(self)
        class(tab_window_configuration_type), intent(inout) :: self
        type(status_type) :: status

        call destroy_window_configuration(self%window_configuration)
        if (self%point%index /= 0) call destroy_marker(self%point, status)
        self%point = null_handle()
    end subroutine destroy_tab_window_configuration

    !> Free the window configurations and markers of tab SELF.
    !> Not a port.
    subroutine destroy_tab(self)
        class(tab_type), intent(inout) :: self
        integer :: k

        if (self%has_window_configuration) call self%window_configuration%destroy()
        self%has_window_configuration = .false.

        if (allocated(self%history_back)) then
            do k = 1, size(self%history_back)
                call self%history_back(k)%destroy()
            end do

            deallocate (self%history_back)
        end if

        if (allocated(self%history_forward)) then
            do k = 1, size(self%history_forward)
                call self%history_forward(k)%destroy()
            end do

            deallocate (self%history_forward)
        end if

        self%has_history = .false.
    end subroutine destroy_tab

    !> The number of lines at the top of frame F above its windows: its
    !> tab bar lines (no menu bar or tool bar on a terminal frame).
    !> Port of frame.h:FRAME_TOP_MARGIN.
    integer function frame_top_margin(f) result(lines)
        type(frame_type), intent(in) :: f

        lines = f%tab_bar_lines
    end function frame_top_margin

    !> Return width (in pixels) of vertical window dividers on FRAME.
    !> Port of frame.c:Fright_divider_width (frame.h:FRAME_RIGHT_DIVIDER_WIDTH).
    integer function frame_right_divider_width(f) result(width)
        type(frame_type), intent(in) :: f

        width = f%right_divider_width
    end function frame_right_divider_width

    !> Return width (in pixels) of horizontal window dividers on FRAME.
    !> Port of frame.c:Fbottom_divider_width (frame.h:FRAME_BOTTOM_DIVIDER_WIDTH).
    integer function frame_bottom_divider_width(f) result(width)
        type(frame_type), intent(in) :: f

        width = f%bottom_divider_width
    end function frame_bottom_divider_width

    !> Return the width in pixels of WINDOW's right divider.
    !> WINDOW must be a live window of `selected_frame'.  Port of
    !> window.c:Fwindow_right_divider_width (window.h:WINDOW_RIGHT_DIVIDER_WIDTH:
    !> none for a window at the frame's right edge, WINDOW_RIGHTMOST_P).
    integer function window_right_divider_width(w) result(width)
        type(window_type), intent(in) :: w

        width = 0
        if (.not. is_window_valid(selected_frame%root_window)) return

        associate (root => windows(selected_frame%root_window%index))
            if (w%left_col + w%total_cols /= root%left_col + root%total_cols) then
                width = frame_right_divider_width(selected_frame)
            end if
        end associate
    end function window_right_divider_width

    !> Return the width in pixels of WINDOW's bottom divider.
    !> WINDOW must be a live window of `selected_frame'.  Port of
    !> window.c:Fwindow_bottom_divider_width (window.h:WINDOW_BOTTOM_DIVIDER_WIDTH:
    !> none for the minibuffer window and a window at the bottom of the root
    !> window, WINDOW_BOTTOMMOST_P, when the frame's minibuffer window is not
    !> below it; with one, the root window has a next sibling).
    integer function window_bottom_divider_width(w) result(width)
        type(window_type), intent(in) :: w

        width = 0
        if (w%mini .or. .not. is_window_valid(selected_frame%root_window)) return

        associate (root => windows(selected_frame%root_window%index))
            ! (The minibuffer window follows the root window: a window at
            ! the bottom of the root window has a divider.)
            if (w%top_line + w%total_lines /= root%top_line + root%total_lines .or. &
                is_window_valid(selected_frame%minibuffer_window)) width = frame_bottom_divider_width(selected_frame)
        end associate
    end function window_bottom_divider_width

    !> Return true if VALUE is a positive number.  Port of
    !> frame.el:window-divider-width-valid-p (an integer).
    elemental logical function is_window_divider_width_valid(value) result(valid)
        integer, intent(in) :: value

        valid = value > 0
    end function is_window_divider_width_valid

    !> The frame's height in lines (frame.h:FRAME_LINES): its terminal's
    !> on a terminal frame.  Port of frame.h:FRAME_LINES.
    integer function frame_lines(f)
        type(frame_type), intent(in) :: f

        if (associated(f%rif)) then
            frame_lines = f%text_lines
        else
            frame_lines = f%tty%wcm%cm_rows
        end if
    end function frame_lines

    !> The frame's width in columns.  Port of frame.h:FRAME_COLS.
    integer function frame_cols(f)
        type(frame_type), intent(in) :: f

        if (associated(f%rif)) then
            frame_cols = f%text_cols
        else
            frame_cols = f%tty%wcm%cm_cols
        end if
    end function frame_cols

    !> The left edge of window `w' on frame `f' in pixels.
    !> Port of window.h:WINDOW_LEFT_EDGE_X, in part.
    integer function window_pixel_left(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_left = w%left_col*f%column_width
    end function window_pixel_left

    !> The top edge of window `w' in pixels.  Port of
    !> window.h:WINDOW_TOP_EDGE_Y, in part.
    integer function window_pixel_top(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_top = w%top_line*f%line_height
    end function window_pixel_top

    !> The width of window `w' in pixels.  Port of
    !> window.h:WINDOW_PIXEL_WIDTH, in part.
    integer function window_pixel_width(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_width = w%total_cols*f%column_width
    end function window_pixel_width

    !> The height of window `w' in pixels, mode line included.
    !> Port of window.h:WINDOW_PIXEL_HEIGHT, in part.
    integer function window_pixel_height(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        window_pixel_height = w%total_lines*f%line_height
    end function window_pixel_height

    !> The width of the text area of window `w' in pixels: on a terminal,
    !> less the column of the border with a window on its right; a
    !> window-system frame draws that border over the window's last pixel
    !> column (there are no fringes, margins, scroll bars or dividers).
    !> Port of window.c:window_body_width (WINDOW_BODY_IN_PIXELS).
    integer function window_body_pixel_width(f, w)
        type(frame_type), intent(in) :: f
        type(window_type), intent(in) :: w

        if (associated(f%rif)) then
            window_body_pixel_width = window_pixel_width(f, w)
        else
            window_body_pixel_width = window_text_cols(w)
        end if
    end function window_body_pixel_width

    !> Return minimum number of lines of FRAME's windows.
    !> HORIZONTAL true means return number of columns of FRAME's windows.
    !> Sizes are what the windows can safely shrink to (IGNORE `safe'),
    !> in lines and columns.  Port of window.el:frame-windows-min-size
    !> (IGNORE `safe', as frame.c:adjust_frame_size asks for it when a
    !> frame size change comes from the terminal), with
    !> window.el:window-min-size, window.el:window--min-size-1 and
    !> window.el:window-safe-min-size (`window_size_fixed' is not ported).
    integer function frame_windows_min_size(f, horizontal) result(n)
        type(frame_type), intent(in) :: f
        logical, intent(in) :: horizontal

        n = safe_min_size(f%root_window)
        ! The minibuffer window: one line (window.el:window-min-pixel-height).
        if (.not. horizontal .and. is_window_valid(f%minibuffer_window)) n = n + max(1, window_safe_min_height)

    contains

        !> The safe minimum size of the tree under `h'.
        recursive integer function safe_min_size(h) result(m)
            type(handle_type), intent(in) :: h
            type(handle_type) :: sub

            m = 0
            if (.not. is_window_valid(h)) return
            sub = windows(h%index)%child

            if (.not. is_window_valid(sub)) then
                m = window_safe_min_height
                if (horizontal) m = window_safe_min_width
            else if (windows(h%index)%horizontal .eqv. horizontal) then
                ! An iso-combination: the sum of its children's sizes.
                do while (is_window_valid(sub))
                    m = m + safe_min_size(sub)
                    sub = windows(sub%index)%next
                end do
            else
                ! An ortho-combination: the largest of its children's sizes.
                do while (is_window_valid(sub))
                    m = max(m, safe_min_size(sub))
                    sub = windows(sub%index)%next
                end do
            end if
        end function safe_min_size
    end function frame_windows_min_size

    !> Resize frame F's windows when F's inner height (inner width if
    !> HORFLAG is true) has been set to SIZE pixels: the root window takes
    !> the frame's new size less the minibuffer window, which becomes one
    !> line high at the bottom.  A root window with children shares the
    !> change among them by their normal and minimum sizes
    !> (`window_resize_root_window'), first respecting `window_min_height'
    !> and `window_min_width', then ignoring them; when neither works the
    !> windows keep their sizes.  Port of window.c:resize_frame_windows
    !> (the root window starts below the tab bar, FRAME_TOP_MARGIN; no menu
    !> bar on a terminal frame; window sizes are whole lines and columns, so window.el's
    !> window--pixel-to-total has nothing to round).
    subroutine resize_frame_windows(f, size, horflag)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: size
        logical, intent(in) :: horflag
        integer :: unit, old_size, old_pixel_size, new_units, new_pixel_size, mini_height, frame_cols
        type(handle_type) :: root, mini

        root = f%root_window
        mini = f%minibuffer_window
        if (.not. is_window_valid(root)) return

        if (horflag) then
            unit = f%column_width
            old_size = windows(root%index)%total_cols
        else
            unit = f%line_height
            old_size = windows(root%index)%total_lines
        end if

        old_pixel_size = old_size*unit
        ! The minibuffer window has no mode line, so all of it is its
        ! body: one line remains.
        mini_height = 0
        if (is_window_valid(mini)) mini_height = unit

        if (horflag) then
            new_pixel_size = max(size, unit)
        else
            new_pixel_size = max(size - mini_height, unit)
        end if

        new_units = new_pixel_size/unit
        frame_cols = windows(root%index)%total_cols
        if (horflag) frame_cols = new_units

        if (new_pixel_size == old_pixel_size .and. (horflag .or. windows(root%index)%top_line == frame_top_margin(f))) then
            ! Nothing to do for the root window.
        else if (.not. is_window_valid(windows(root%index)%child)) then
            ! For a leaf root window just set the size.
            if (horflag) then
                windows(root%index)%total_cols = new_units
                windows(root%index)%divider = .false.
            else
                windows(root%index)%top_line = frame_top_margin(f)
                windows(root%index)%total_lines = new_units
            end if
        else
            if (.not. horflag) windows(root%index)%top_line = frame_top_margin(f)
            ! Try a "normal" resize first.
            call window_resize_root_window(root, new_units - old_size, horflag, .false.)

            if (window_resize_check(root, horflag) .and. new_units == windows(root%index)%new_total) then
                call window_resize_apply(root, horflag, frame_cols)
            else
                ! Try with "reasonable" minimum sizes next.
                call window_resize_root_window(root, new_units - old_size, horflag, .true.)

                if (window_resize_check(root, horflag) .and. new_units == windows(root%index)%new_total) then
                    call window_resize_apply(root, horflag, frame_cols)
                end if
            end if
        end if

        if (.not. is_window_valid(mini)) return

        associate (m => windows(mini%index), r => windows(root%index))
            if (horflag) then
                m%total_cols = new_units
            else
                m%total_lines = mini_height/unit
                m%top_line = r%top_line + r%total_lines
            end if
        end associate
    end subroutine resize_frame_windows

    ! ------------------------------------------------------------------
    ! The frame registry
    ! ------------------------------------------------------------------

    !> Register the initial frame, F1, if no frame is registered yet.
    !> Port of frame.c:make_initial_frame (its frame list and name).
    subroutine ensure_frames()
        type(frame_type), pointer :: f

        if (frame_table%count() > 0) return
        f => initial_frame
        call register_frame(f)
        tty_frame_count = 1
        top_frame = f%handle
        f%name = "F1"
        f%explicit_name = .false.
        f%visible = .true.
    end subroutine ensure_frames

    !> Give frame F a slot of the registry and put it first in the frame
    !> order (frame.c:make_terminal_frame's `Vframe_list = Fcons (frame,
    !> Vframe_list)').  Not a port.
    subroutine register_frame(f)
        type(frame_type), pointer, intent(in) :: f
        type(frame_pointer_type), allocatable :: grown(:)
        type(handle_type), allocatable :: order(:)

        call frame_table%acquire(f%handle)
        if (.not. allocated(frame_slots)) allocate (frame_slots(0))

        if (size(frame_slots) < frame_table%capacity()) then
            allocate (grown(frame_table%capacity()))
            grown(1:size(frame_slots)) = frame_slots
            call move_alloc(grown, frame_slots)
        end if

        frame_slots(f%handle%index)%frame => f
        if (.not. allocated(frame_order)) allocate (frame_order(0))
        allocate (order(size(frame_order) + 1))
        order(1) = f%handle
        order(2:) = frame_order
        call move_alloc(order, frame_order)
    end subroutine register_frame

    !> Take frame F out of the registry: its slot is released (its
    !> handles become stale) and it leaves the frame order.  Not a port.
    subroutine unregister_frame(f)
        type(frame_type), intent(inout) :: f
        type(status_type) :: status
        integer(int64) :: i

        i = f%handle%index
        frame_order = pack(frame_order, frame_order%index /= i)
        call frame_table%release(f%handle, status)
        if (status%ok()) frame_slots(i)%frame => null()
    end subroutine unregister_frame

    !> Return true if FRAME is a frame that has not been deleted.
    !> Port of frame.c:Fframe_live_p (a terminal frame).
    logical function is_frame_live(frame)
        type(handle_type), intent(in) :: frame

        call ensure_frames()
        is_frame_live = frame_table%is_live(frame)
    end function is_frame_live

    !> The live frame FRAME, or a null pointer if it was deleted.
    !> Not a port.
    function get_frame(frame) result(f)
        type(handle_type), intent(in) :: frame
        type(frame_type), pointer :: f

        f => null()
        if (is_frame_live(frame)) f => frame_slots(frame%index)%frame
    end function get_frame

    !> Return a list of all live frames.  The return value does not
    !> include any tooltip frame.  Port of frame.c:Fframe_list (the newest
    !> frame first, as Vframe_list holds them).
    function frame_list() result(frames)
        type(handle_type), allocatable :: frames(:)

        call ensure_frames()
        frames = frame_order
    end function frame_list

    !> Return the next frame in the frame list after FRAME.
    !> Only frames on the same terminal as FRAME are included in the list
    !> of candidate frames.  FRAME defaults to the selected frame.
    !> If there is no other frame, FRAME is returned.  Port of
    !> frame.c:Fnext_frame and frame.c:next_frame (MINIFRAME absent: all
    !> frames but minibuffer-only ones, which a terminal does not have;
    !> one terminal, so frame.c:candidate_frame takes them all).
    function next_frame(frame) result(next)
        type(handle_type), intent(in), optional :: frame
        type(handle_type) :: next
        integer :: k

        call ensure_frames()
        next = selected_frame%handle
        if (present(frame)) next = frame
        k = findloc(frame_order%index, next%index, dim=1)
        if (k == 0) return
        next = frame_order(modulo(k, size(frame_order)) + 1)
    end function next_frame

    !> Return the previous frame in the frame list before FRAME.
    !> It considers only frames on the same terminal as FRAME.  FRAME
    !> defaults to the selected frame.  If there is no other frame, FRAME
    !> is returned.  Port of frame.c:Fprevious_frame and
    !> frame.c:prev_frame (MINIFRAME absent, one terminal).
    function previous_frame(frame) result(previous)
        type(handle_type), intent(in), optional :: frame
        type(handle_type) :: previous
        integer :: k

        call ensure_frames()
        previous = selected_frame%handle
        if (present(frame)) previous = frame
        k = findloc(frame_order%index, previous%index, dim=1)
        if (k == 0) return
        previous = frame_order(modulo(k - 2, size(frame_order)) + 1)
    end function previous_frame

    !> Return a frame named NAME, or a null handle if there is none.
    !> Port of frame.c:frame_get.
    function find_frame(name) result(frame)
        character(len=*), intent(in) :: name
        type(handle_type) :: frame
        integer :: k

        call ensure_frames()
        frame = null_handle()

        do k = 1, size(frame_order)
            associate (f => frame_slots(frame_order(k)%index)%frame)
                if (allocated(f%name)) then
                    if (f%name == name .and. len(f%name) == len(name)) then
                        frame = frame_order(k)
                        return
                    end if
                end if
            end associate
        end do
    end function find_frame

    !> Return the frame that window WINDOW is on (a valid window: live or
    !> internal), or a null handle if it is on none (a window deleted with
    !> its frame).  Port of window.c:Fwindow_frame (the frame found from
    !> the window's root window, as upstream's w->frame).
    function window_frame(window) result(frame)
        type(handle_type), intent(in) :: window
        type(handle_type) :: frame
        type(handle_type) :: root
        integer :: k

        call ensure_frames()
        frame = null_handle()
        if (.not. is_window_valid(window)) return
        root = window

        do while (is_window_valid(windows(root%index)%parent))
            root = windows(root%index)%parent
        end do

        do k = 1, size(frame_order)
            associate (f => frame_slots(frame_order(k)%index)%frame)
                if (same_handle(f%root_window, root) .or. same_handle(f%minibuffer_window, root)) then
                    frame = frame_order(k)
                    return
                end if
            end associate
        end do
    end function window_frame

    !> Whether STRING has the form F<num>.  Port of
    !> frame.c:frame_name_fnn_p.
    pure logical function is_frame_name_fnn(string)
        character(len=*), intent(in) :: string

        is_frame_name_fnn = .false.
        if (len(string) < 2) return
        if (string(1:1) /= "F") return
        is_frame_name_fnn = verify(string(2:), "0123456789") == 0
    end function is_frame_name_fnn

    !> The next free name of the form F<num> for a terminal frame.
    !> Port of frame.c:frame_next_F_name.
    function get_next_frame_name() result(name)
        character(len=:), allocatable :: name
        character(len=24) :: digits

        do
            tty_frame_count = tty_frame_count + 1
            write (digits, "(i0)") tty_frame_count
            name = "F"//trim(digits)
            if (.not. is_frame_live(find_frame(name))) return
        end do
    end function get_next_frame_name

    !> Set the name of the terminal frame F to NAME, or when NAME is
    !> absent to F<num>.  Port of frame.c:set_term_frame_name.
    subroutine set_term_frame_name(f, status, name)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name

        status = status_type()
        f%explicit_name = present(name)

        if (.not. present(name)) then
            ! Check for no change needed in this very common case.
            if (allocated(f%name)) then
                if (is_frame_name_fnn(f%name)) return
            end if

            f%name = get_next_frame_name()
            return
        end if

        ! Don't change the name if it's already NAME.
        if (allocated(f%name)) then
            if (f%name == name .and. len(f%name) == len(name)) return
        end if

        ! Stop the user setting the name to F<num> if it is already in use.
        if (is_frame_name_fnn(name)) then
            if (is_frame_live(find_frame(name))) then
                call set_error(status, FM_GENERIC_ERROR, "Frame of the form F<num> named `"//name//"' already exists")
                return
            end if
        end if

        f%name = name
    end subroutine set_term_frame_name

    !> Make `selected_frame''s windows if they don't exist yet: one window
    !> (the root, selected) and the minibuffer window, sized later by
    !> xdisp_mod:init_frame_windows.  Port of window.c:init_window_once
    !> (the initial frame's windows).
    subroutine ensure_frame_windows()
        call ensure_frames()

        if (.not. is_window_valid(selected_frame%root_window)) then
            call make_window(selected_frame%root_window)
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. is_window_valid(selected_frame%selected_window)) then
            selected_frame%selected_window = selected_frame%root_window
        end if

        if (.not. is_window_valid(selected_frame%minibuffer_window)) then
            call make_window(selected_frame%minibuffer_window)
            windows(selected_frame%minibuffer_window%index)%mini = .true.
        end if
    end subroutine ensure_frame_windows

    !> Return the selected window.  The selected window is the window in
    !> which the standard cursor for selected windows appears and to
    !> which many commands apply.  Port of window.c:Fselected_window.
    function selected_window() result(h)
        type(handle_type) :: h

        call ensure_frame_windows()

        if (selected_frame%minibuffer_selected) then
            h = selected_frame%minibuffer_window
        else
            h = selected_frame%selected_window
        end if
    end function selected_window

    !> The selected window of `selected_frame': the minibuffer window while
    !> it is selected, else `selected_window'.  window.c:selected_window.
    !> Not a port.
    function selected_window_ref() result(w)
        type(window_type), pointer :: w

        if (selected_frame%minibuffer_selected) then
            w => minibuffer_window_ref()
        else
            w => frame_selected_window_ref()
        end if
    end function selected_window_ref

    !> The frame's selected window (not the minibuffer window): the one
    !> showing the current buffer, or that the minibuffer was entered from.
    !> Valid until a window is made.  Not a port.
    function frame_selected_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%selected_window)
    end function frame_selected_window_ref

    !> The minibuffer window.  Valid until a window is made.  Not a port.
    function minibuffer_window_ref() result(w)
        type(window_type), pointer :: w

        call ensure_frame_windows()
        w => window_ref(selected_frame%minibuffer_window)
    end function minibuffer_window_ref
end module frame_mod
