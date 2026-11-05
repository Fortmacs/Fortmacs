! window_mod.f90 -- windows: a buffer shown in part of a frame.
! Ported from GNU Emacs 31.1 src/window.c and src/window.h (the parts in
! use).
!
! Copyright (C) 1985-1987, 1993-1998, 2000-2026 Free Software Foundation,
! Inc.
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

!> A frame has a root window (no splitting yet) and a one-line minibuffer
!> window, which shows the echo area.  A window's start and point are
!> markers, so they follow insertions and deletions in its buffer.
module window_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use execute_interactively_mod, only: prefix_arg_type
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle, slot_table_type, same_handle
    use hooks_mod, only: window_position_hook_type
    use buffer_mod, only: buffers, resolve_buffer
    use data_mod, only: choice_setting_type
    use marker_mod, only: make_marker, destroy_marker, set_marker, marker_position
    use hooks_mod, only: hook_type
    use value_mod, only: command_procedure
    implicit none(type, external)
    private

    public :: set_window_buffer, set_window_buffer_internal, window_discard_buffer_from_window, record_window_buffer, &
              switch_to_prev_buffer, switch_to_next_buffer, previous_buffer, next_buffer, quit_restore_window, bury_buffer, &
              display_buffer_record_window, display_buffer, pop_to_buffer, pop_to_buffer_same_window, fit_window_to_buffer, &
              shrink_window_if_larger_than_buffer, display_buffer_no_window, &
              switch_to_buffer_other_window, switch_to_buffer_other_frame, display_buffer_other_frame, &
              window_in_direction, &
              temp_buffer_window_show, scroll_window, get_buffer_window, get_lru_window, get_mru_window, get_largest_window, &
              split_window_and_copy, is_window_combined, is_window_full_height, is_window_splittable, &
              copy_quit_restore, window_resize, init_window_display, &
              current_window_configuration, set_window_configuration, destroy_window_configuration, &
              get_window_state, set_window_state, destroy_window_state, &
              window_display_buffer, &
              copy_window_configuration, is_window_configuration_equal, execute_window_change_functions, &
              execute_window_configuration_change_hook
    public :: window_start, set_window_start, window_point, &
              set_window_point, window_body_lines, window_wants_header_line, window_wants_tab_line, destroy_window, &
              destroy_previous_buffer, destroy_quit_restore, unshow_buffer
    public :: make_window, delete_window_object, window_ref, is_window_live, is_window_valid, same_window, same_handle, &
              window_leaves, resize_window_tree, window_text_cols, split_window_internal, &
              remove_window_from_tree, replace_window, delete_window_tree
    public :: window_resize_child_windows
    public :: window_resize_reset, window_resize_this_window, window_resize_root_window, window_sizable, &
              is_window_sizable, window_min_delta, window_resize_check, window_resize_apply, window_normal_size
    public :: scroll_up, scroll_down, recenter, move_to_window_line, &
              scroll_up_command, scroll_down_command, recenter_top_bottom, &
              is_pos_visible_in_window, other_window_for_scrolling, scroll_other_window, scroll_other_window_down, &
              move_to_window_line_top_bottom, quit_window, quit_windows_on, init_window_commands, window_end, &
              execute_window_scroll_functions
    public :: window_min_size
    public :: is_window, window_buffer, window_parent, window_top_child, window_left_child, window_next_sibling, &
              window_prev_sibling, frame_root_window, frame_selected_window, minibuffer_window, is_window_minibuffer, &
              minibuffer_selected_window, window_total_height, window_total_width, window_left_column, window_top_line, &
              window_body_height, window_body_width, window_wants_mode_line, window_mode_line_height, &
              window_header_line_height, window_tab_line_height, window_hscroll, window_dedication, &
              set_window_dedication, window_use_time, window_bump_use_time, window_prev_buffers, window_next_buffers, &
              next_window, previous_window, window_list_1, window_list, window_part_at, window_at, &
              force_window_update
    public :: select_window, split_window, split_window_below, split_window_right, delete_window, delete_other_windows, &
              other_window, other_buffer, &
              replace_buffer_in_windows, switch_to_buffer, init_window_cmds
    public :: window_main_window, window_with_side, walk_window_tree, is_window_no_other, check_window_sides, &
              is_window_sides_reversed_on_frame, display_buffer_in_side_window

    !> Number of lines of continuity when scrolling by screenfuls.
    !> Port of window.c next-screen-context-lines.
    integer, public :: next_screen_context_lines = 2

    !> Move point on scroll error if true.
    !> If false, an error condition occurs in scroll commands when no more scrolling is
    !> possible.  If true, they move point to the beginning or end of
    !> the buffer instead (and then an error condition occurs at the edge).  Port of
    !> window.el scroll-error-top-bottom.
    logical, public :: scroll_error_top_bottom = .false.

    !> List of functions to call before redisplaying a window with scrolling.
    !> Each function is called with two arguments, the window and its new
    !> display-start position.
    !> These functions are called whenever the `window_start' marker is modified,
    !> either to point into another buffer (e.g. via `set_window_buffer') or another
    !> place in the same buffer.
    !> When each function is called, the `window_start' marker of its window
    !> argument has been already set to the new value, and the buffer which that
    !> window will display is set to be the current buffer.
    !> Note that the value of `window_end' is not valid when these functions are
    !> called.
    !>
    !> Warning: Do not use this feature to alter the way the window
    !> is scrolled.  It is not designed for that, and such use probably won't
    !> work.  Port of xdisp.c window-scroll-functions.
    type(window_position_hook_type), target, public :: window_scroll_functions = &
                                                       window_position_hook_type(name="window_scroll_functions")

    !> A buffer a window showed before (window.h prev_buffers element):
    !> its window start and point when it was removed, as markers.
    type, public :: prev_buffer_type
        type(handle_type) :: buffer, start, point
    end type prev_buffer_type

    !> What a window's `quit_restore' parameter records (see
    !> window.el:display-buffer-record-window): how the window came to
    !> show `buffer' (QR_WINDOW, QR_FRAME, QR_TAB, QR_SAME or QR_OTHER;
    !> QR_NONE for no parameter), what to restore (QR_WINDOW, QR_FRAME,
    !> QR_TAB, or with
    !> `has_quad' the buffer shown before with its start, point and
    !> size), and the window selected at the time.
    type, public :: quit_restore_type
        integer :: kind = 0
        integer :: what = 0
        logical :: has_quad = .false.
        type(handle_type) :: quad_buffer, quad_point
        integer(int64) :: quad_start = 0
        !> The window's height or width then; -1 for none.
        integer :: quad_size = -1
        type(handle_type) :: selected, buffer
    end type quit_restore_type

    !> Kinds of `quit_restore' parameter.
    integer, parameter, public :: QR_NONE = 0, QR_WINDOW = 1, QR_FRAME = 2, QR_SAME = 3, QR_OTHER = 4, QR_TAB = 5

    !> Called by `quit_window' and the other callers of window.el's
    !> window--delete to close the selected frame's current tab when the
    !> window to delete is its last one (tab-bar.el's tab-bar-close-tab,
    !> set by tab_bar_mod: the windows are below the tab bar).
    procedure(command_procedure), pointer, public :: close_tab_hook => null()

    !> Dedication of a window to its buffer: none, weak or strong
    !> (DEDICATED_NONE, DEDICATED_WEAK and DEDICATED_STRONG), or
    !> DEDICATED_SIDE (`side'), weak dedication that a side window keeps
    !> when it switches to another buffer.
    integer, parameter, public :: DEDICATED_NONE = 0, DEDICATED_WEAK = 1, DEDICATED_STRONG = 2, DEDICATED_SIDE = 3

    !> The side of the frame a side window is on (its `window_side'
    !> parameter): WINDOW_SIDE_NONE for a window that is not a side
    !> window, else left, top, right or bottom.  The four sides number 1
    !> to 4 in that order.
    integer, parameter, public :: WINDOW_SIDE_NONE = 0, WINDOW_SIDE_LEFT = 1, WINDOW_SIDE_TOP = 2, &
                                  WINDOW_SIDE_RIGHT = 3, WINDOW_SIDE_BOTTOM = 4

    !> The value of `window_sides_reversed' besides .true. (always reverse)
    !> and .false. (never): reverse when the frame shows right-to-left
    !> text.
    integer, parameter, public :: WINDOW_SIDES_REVERSED_BIDI = 2

    !> No bound on the number of side windows on a side
    !> (`window_sides_slots').
    integer, parameter, public :: WINDOW_SIDES_SLOTS_UNLIMITED = huge(0)

    !> What `quit_restore_window' and `switch_to_prev_buffer' do with the
    !> window's buffer (BURY-OR-KILL): nothing, `append', `bury', `kill',
    !> `killing' or `burying'.
    integer, parameter, public :: BK_NONE = 0, BK_APPEND = 1, BK_BURY = 2, BK_CLOSE = 3, BK_CLOSING = 4, &
                                  BK_BURYING = 5

    !> Buffer display action functions for `display_buffer' (window.el
    !> display-buffer-same-window, display-buffer--maybe-same-window,
    !> display-buffer-reuse-window, display-buffer-pop-up-window,
    !> display-buffer--maybe-pop-up-window, display-buffer-in-previous-window,
    !> display-buffer-use-some-window, display-buffer-at-bottom and
    !> display-buffer-below-selected, display-buffer-in-side-window and
    !> display-buffer-pop-up-frame).
    integer, parameter, public :: DISPLAY_ACTION_SAME_WINDOW = 1, DISPLAY_ACTION_MAYBE_SAME_WINDOW = 2, &
                                  DISPLAY_ACTION_REUSE_WINDOW = 3, DISPLAY_ACTION_POP_UP_WINDOW = 4, &
                                  DISPLAY_ACTION_MAYBE_POP_UP_WINDOW = 5, DISPLAY_ACTION_IN_PREVIOUS_WINDOW = 6, &
                                  DISPLAY_ACTION_USE_SOME_WINDOW = 7, DISPLAY_ACTION_AT_BOTTOM = 8, &
                                  DISPLAY_ACTION_BELOW_SELECTED = 9, DISPLAY_ACTION_IN_SIDE_WINDOW = 10, &
                                  DISPLAY_ACTION_POP_UP_FRAME = 11

    !> Which frames `display_buffer_reuse_window' searches (an action
    !> table's `reusable_frames' entry): the selected frame, all frames,
    !> the visible frames, or the visible and iconified frames; on a
    !> terminal every frame is visible.
    integer, parameter, public :: REUSABLE_FRAMES_SELECTED = 0, REUSABLE_FRAMES_ALL = 1, REUSABLE_FRAMES_VISIBLE = 2, &
                                  REUSABLE_FRAMES_VISIBLE_OR_ICONIFIED = 3

    !> A function that sets the height of a window `display_buffer' made
    !> (the `window_height' entry of an action table).
    abstract interface
        ! Not a port.
        subroutine window_height_procedure(window)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: window
        end subroutine window_height_procedure
    end interface

    !> The height `display_buffer' gives a window it made (the
    !> `window_height' entry of an action table, R6-39): a number of
    !> `lines', a `fraction' of the frame's root window height, or a
    !> `procedure' called with the window to set it; none set means no
    !> entry.
    type, public :: display_window_height_type
        integer :: lines = 0
        real(real64) :: fraction = 0.0_real64
        procedure(window_height_procedure), pointer, nopass :: procedure => null()
    end type display_window_height_type

    !> The width `display_buffer' gives a window it made (the
    !> `window_width' entry): a number of `columns' or a `fraction' of
    !> the frame's root window width; none set means no entry.
    type, public :: display_window_width_type
        integer :: columns = 0
        real(real64) :: fraction = 0.0_real64
    end type display_window_width_type

    !> A buffer display action table: the entries ported.  Port of the
    !> `inhibit_same_window', `allow_no_window', `reusable_frames',
    !> `window_height' (a function, a number of lines or a fraction of the
    !> frame's height), `window_width' (a number of columns or a fraction),
    !> `dedicated', `side', `slot' and `body_function' entries.
    type, public :: display_action_parameters_type
        !> Don't use the selected window.
        logical :: inhibit_same_window = .false.
        !> The caller can cope with no window: `display_buffer_no_window'
        !> displays none.
        logical :: allow_no_window = .false.
        !> The frames to search for a window showing the buffer
        !> (REUSABLE_FRAMES_*).
        integer :: reusable_frames = REUSABLE_FRAMES_SELECTED
        !> The height to give a window made to display the buffer
        !> (`window_height'): its `lines', `fraction' or `procedure'.
        type(display_window_height_type) :: window_height
        !> The width to give such a window (`window_width'): its
        !> `columns' or `fraction'.
        type(display_window_width_type) :: window_width
        !> With `has_dedicated', the dedication (DEDICATED_*) to give the
        !> window chosen, if it is new or a side window.
        logical :: has_dedicated = .false.
        integer :: dedicated = DEDICATED_NONE
        !> For `display_buffer_in_side_window': the side of the frame
        !> (WINDOW_SIDE_*) and the slot on that side: 0 for the middle,
        !> negative before it (above or to the left), positive after it.
        integer :: side = WINDOW_SIDE_BOTTOM
        integer :: slot = 0
        !> Called with the window chosen to display the buffer (the buffer
        !> current, read-only text and modification hooks inhibited), to
        !> fill it before its size is adjusted.
        procedure(window_height_procedure), pointer, nopass :: body_function => null()
    end type display_action_parameters_type

    !> An action function of a caller's own: display BUFFER with TABLE and
    !> give the WINDOW (the null handle to let the next action try);
    !> FAILED true makes `display_buffer' give up and display no window.
    abstract interface
        ! Not a port.
        recursive subroutine display_action_function_procedure(buffer, table, window, failed, status)
            import :: handle_type, display_action_parameters_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(display_action_parameters_type), intent(in) :: table
            type(handle_type), intent(out) :: window
            logical, intent(out) :: failed
            type(status_type), intent(out) :: status
        end subroutine display_action_function_procedure
    end interface

    !> A buffer display action given in full: action functions
    !> (DISPLAY_ACTION_*, and `action_function', tried first, when not
    !> null) and the table entries it sets (`inhibit_same_window' when
    !> `has_inhibit_same_window', `allow_no_window' when
    !> `has_allow_no_window').
    type, public :: display_action_type
        integer, allocatable :: actions(:)
        logical :: has_inhibit_same_window = .false.
        logical :: inhibit_same_window = .false.
        logical :: has_allow_no_window = .false.
        logical :: allow_no_window = .false.
        procedure(display_action_function_procedure), pointer, nopass :: action_function => null()
    end type display_action_type

    !> A buffer display action that overrides all others.
    !> If non-empty, this action takes precedence over all others: its
    !> functions are tried first and the entries of its table win over
    !> those of the other actions.  This variable is not intended for
    !> user customization.  Programs should never set this variable
    !> permanently but may bind it around calls of buffer display
    !> functions like `display_buffer' or `pop_to_buffer'.  Port of
    !> window.el display-buffer-overriding-action.
    type(display_action_type), public :: display_buffer_overriding_action

    !> The window to scroll with \[scroll_other_window] from the
    !> minibuffer: the window showing completions (the null handle for
    !> none).  Port of window.c minibuffer-scroll-window.
    type(handle_type), public :: minibuffer_scroll_window

    !> If not DEDICATED_NONE, `display_buffer' marks the windows it creates as
    !> dedicated.  The actual value of this variable will be copied
    !> to the `window_dedication' flag.  Port of window.el
    !> display-buffer-mark-dedicated (DEDICATED_NONE, DEDICATED_WEAK or
    !> DEDICATED_STRONG).
    integer, public :: display_buffer_mark_dedicated = DEDICATED_NONE

    !> If true, left and right side windows occupy full frame height.
    !> If false, top and bottom side windows occupy full frame width.
    !> Port of window.el window-sides-vertical.
    logical, public :: window_sides_vertical = .false.

    !> Whether top/bottom side windows appear in reverse order.
    !> When this is .false., side windows on the top and
    !> bottom of a frame are always drawn from left to right with
    !> increasing slot values.  When this is .true.,
    !> side windows on the top and bottom of a frame are always drawn
    !> from right to left with increasing slot values.
    !>
    !> When this is WINDOW_SIDES_REVERSED_BIDI, the drawing order is like
    !> that for .true. if the value of
    !> `bidi_paragraph_direction' is `right-to-left' in the buffer most
    !> recently shown in the window selected within the main window area
    !> of this frame.
    !>
    !> The layout of side windows on the left or right of a frame is not
    !> affected by the value of this variable.  Port of window.el
    !> window-sides-reversed.
    type(choice_setting_type), public :: window_sides_reversed = &
                                         choice_setting_type(name="window_sides_reversed")

    !> The number of side window slots on each side of a frame
    !> (`window_sides_slots', R6-39): `left', `top', `right' and
    !> `bottom', each a number or WINDOW_SIDES_SLOTS_UNLIMITED.
    type, public :: window_sides_slots_type
        integer :: left = WINDOW_SIDES_SLOTS_UNLIMITED
        integer :: top = WINDOW_SIDES_SLOTS_UNLIMITED
        integer :: right = WINDOW_SIDES_SLOTS_UNLIMITED
        integer :: bottom = WINDOW_SIDES_SLOTS_UNLIMITED
    end type window_sides_slots_type

    !> Number of available side window slots on each side of a frame.
    !> The value has four components specifying the maximum number of
    !> side windows that may be created on the `left', `top', `right'
    !> and `bottom' side of any frame.
    !>
    !> If a component is a number, `display_buffer_in_side_window' will
    !> refrain from making a new side window if the number of windows on
    !> that side is equal to or exceeds that number.  Rather, it will
    !> reuse the window whose `window_slot' value is nearest to the slot
    !> specified via its table argument.  If a component is
    !> WINDOW_SIDES_SLOTS_UNLIMITED, this means there's no bound on the
    !> number of windows on that side.  Port of window.el
    !> window-sides-slots.
    type(window_sides_slots_type), public :: window_sides_slots

    !> If true, standard functions ignore window parameters.
    !> The functions currently affected by this are `split_window',
    !> `delete_window', `delete_other_windows' and `other_window'.
    !>
    !> An application may set this to true around calls to these
    !> functions to inhibit processing of window parameters.  Port of
    !> window.el ignore-window-parameters.
    logical, public :: ignore_window_parameters = .false.

    !> True means inhibit any checks on side windows (window.el
    !> window--sides-inhibit-check).
    logical :: window_sides_inhibit_check = .false.

    !> Which windows `next_window' and its relatives consider besides the
    !> ordinary ones (their MINIBUF argument): the minibuffer window if the
    !> minibuffer is active (MINIBUF_ACTIVE), always (MINIBUF_YES), or never (MINIBUF_NO).
    integer, parameter, public :: MINIBUF_ACTIVE = 0, MINIBUF_YES = 1, MINIBUF_NO = 2

    !> The directions `window_in_direction' looks in (its DIRECTION
    !> argument: `above' or `up', `below' or `down', `left', `right').
    integer, parameter, public :: DIRECTION_ABOVE = 1, DIRECTION_BELOW = 2, DIRECTION_LEFT = 3, DIRECTION_RIGHT = 4

    !> Functions called during redisplay when window configuration has changed.
    !> The value should be a list of functions that take no argument.
    !>
    !> Functions specified buffer-locally are called for each window showing
    !> the corresponding buffer if at least one window on that frame has been
    !> added, deleted or changed its buffer or its total or body size since
    !> the last redisplay.  Each call is performed with the window showing
    !> the buffer temporarily selected.
    !>
    !> Functions specified by the default value are called for each frame if
    !> at least one window on that frame has been added, deleted or changed
    !> its buffer or its total or body size since the last redisplay.  Each
    !> call is performed with the frame temporarily selected.  Port of
    !> window.c window-configuration-change-hook.
    type(hook_type), target, public :: window_configuration_change_hook = &
                                       hook_type(name="window_configuration_change_hook")

    !> The parts of a window `window_part_at' tells (window.h
    !> enum window_part, the ones a terminal frame has).
    integer, parameter, public :: ON_NOTHING = 0, ON_TEXT = 1, ON_MODE_LINE = 2, ON_VERTICAL_BORDER = 3, &
                                  ON_HEADER_LINE = 4, ON_TAB_LINE = 5

    !> Values of `split_window_preferred_direction'.
    integer, parameter, public :: SPLIT_WINDOW_PREFERRED_DIRECTION_LONGEST = 2, SPLIT_WINDOW_PREFERRED_DIRECTION_VERTICAL = 3, &
                                  SPLIT_WINDOW_PREFERRED_DIRECTION_HORIZONTAL = 4

    !> Minimum height for splitting windows sensibly.
    !> If this is an integer, `split_window_sensibly' may split a window
    !> vertically only if it has at least this many lines.  If, however, a
    !> window is the only window on its frame, or all the other ones are
    !> dedicated, `split_window_sensibly' may split it vertically
    !> disregarding the value of this variable.  Port of window.el
    !> split-height-threshold.
    integer, public :: split_height_threshold = 80

    !> Minimum width for splitting windows sensibly.
    !> If this is an integer, `split_window_sensibly' may split a window
    !> horizontally only if it has at least this many columns.  Port of
    !> window.el split-width-threshold.
    integer, public :: split_width_threshold = 150

    !> The first direction tried when Fortmacs needs to split a window.
    !> This variable controls in which order `split_window_sensibly' will
    !> try to split the window.  If set to `vertical' (SPLIT_WINDOW_PREFERRED_DIRECTION_VERTICAL),
    !> `split_window_sensibly' tries to split vertically first and then
    !> horizontally.  If set to `horizontal' (SPLIT_WINDOW_PREFERRED_DIRECTION_HORIZONTAL) it does the
    !> opposite.  If set to `longest' (SPLIT_WINDOW_PREFERRED_DIRECTION_LONGEST, the default), the
    !> first direction tried depends on the frame shape: in landscape
    !> orientation it will be like `horizontal', but in portrait it will be
    !> like `vertical'.  Port of window.el split-window-preferred-direction.
    type(choice_setting_type), public :: split_window_preferred_direction = &
                                         choice_setting_type(name="split_window_preferred_direction", &
                                           true_valid=.false., false_valid=.false., choice=SPLIT_WINDOW_PREFERRED_DIRECTION_LONGEST)

    !> True means `display_buffer' should make a new window.  Port of
    !> window.el pop-up-windows.
    logical, public :: pop_up_windows = .true.

    !> If true `display_buffer' will try to even window sizes.
    !> Otherwise `display_buffer' will leave the window configuration
    !> alone.  Port of window.el even-window-sizes (`height_only' and
    !> `width_only' are deferred).
    logical, public :: even_window_sizes = .true.

    !> The minimum total height, in lines, of any window.
    !> The value has to accommodate one text line, a mode and header line,
    !> a horizontal scroll bar and a bottom divider, if present.  A value
    !> less than `window_safe_min_height' is ignored.  Port of window.el
    !> window-min-height.
    integer, public :: window_min_height = 4

    !> The minimum total width, in columns, of any window.
    !> The value has to accommodate two text columns as well as margins,
    !> fringes, a scroll bar and a right divider, if present.  A value less
    !> than `window_safe_min_width' is ignored.  Port of window.el
    !> window-min-width.
    integer, public :: window_min_width = 10

    !> The absolute minimum number of lines and columns of any window
    !> (window.el window-safe-min-height, window-safe-min-width).
    integer, parameter, public :: window_safe_min_height = 1, window_safe_min_width = 2

    !> The use time of the most recently selected window (window.c
    !> window_select_count).
    integer(int64) :: window_select_count = 0

    !> How far a window's tab line is scrolled (its `tab_line_hscroll'
    !> parameter): the index from 0 of the first tab shown, set by the
    !> user (`tab', upstream an integer) or by the automatic scrolling of
    !> the tab line (`automatic_tab', upstream a float); both negative
    !> for none.  Not a port.
    type, public :: tab_line_hscroll_type
        integer :: tab = -1
        integer :: automatic_tab = -1
    end type tab_line_hscroll_type

    !> The window parameters of tab-line.el (tab_line_mod).  Not a port.
    type, public :: window_tab_line_type
        !> The `tab_line_buffers' parameter, a persistent one: the
        !> buffers of the tabs in their order, kept by
        !> `get_tab_line_fixed_window_buffers' (unallocated for none).
        type(handle_type), allocatable :: buffers(:)
        !> The `tab_line_hscroll' parameter.
        type(tab_line_hscroll_type) :: hscroll
        !> The `tab_line_group' parameter: the group whose buffers the tab
        !> line shows (unallocated for none).
        character(len=:), allocatable :: group
        !> The `tab_line_groups' parameter: the tab line shows the groups.
        logical :: groups = .false.
    end type window_tab_line_type

    !> Port of the parts of window.h:struct window in use.
    type, public :: window_type
        !> The buffer shown (contents); the null handle for none.
        type(handle_type) :: buffer
        !> Markers: where the display starts, and point when not selected.
        type(handle_type) :: start, pointm
        !> Position on the frame (0-based rows) and size, in lines and
        !> columns, including the mode line.
        integer :: top_line = 0, total_lines = 0, left_col = 0, total_cols = 0
        !> The buffers shown before, most recently removed first
        !> (`window_prev_buffers').
        type(prev_buffer_type), allocatable :: prev_buffers(:)
        !> Buffers recently replaced by `switch_to_prev_buffer'
        !> (`window_next_buffers').
        type(handle_type), allocatable :: next_buffers(:)
        !> The `quit_restore' and `quit_restore_prev' parameters.
        type(quit_restore_type) :: quit_restore, quit_restore_prev
        !> Whether the window is dedicated to its buffer.
        integer :: dedicated = DEDICATED_NONE
        !> The `window_side' parameter: the side of the frame (WINDOW_SIDE_*)
        !> of a side window, or of an internal window whose children are
        !> the side windows on that side; WINDOW_SIDE_NONE for others.
        integer :: window_side = WINDOW_SIDE_NONE
        !> The `window_slot' parameter of a side window: its slot on its
        !> side (0 the middle, negative before it, positive after it).
        integer :: window_slot = 0
        !> The `no_other_window' parameter: `other_window' and the
        !> functions that look for an "other" window skip this one.
        logical :: no_other_window = .false.
        !> The `no_delete_other_windows' parameter: `delete_other_windows'
        !> leaves this window alone.
        logical :: no_delete_other_windows = .false.
        !> The window tree: the parent (internal) window, the next and
        !> previous siblings, and for an internal window its first child;
        !> null handles for none.  Port of window.h parent, next, prev and
        !> contents (a window with a child shows no buffer).
        type(handle_type) :: parent, next, prev, child
        !> An internal window whose children are side by side (a horizontal
        !> combination); else they are one above the other.
        logical :: horizontal = .false.
        !> The window's share of its parent's height and width (window.h
        !> normal_lines, normal_cols): 1.0 for a root window and across a
        !> combination; the children of a combination share 1.0 along it.
        !> A frame size change gives each window of a combination its part
        !> by them, so that shrinking and growing back restores the sizes.
        real(real64) :: normal_lines = 1.0_real64, normal_cols = 1.0_real64
        !> The size a resizing gives the window, in lines or columns
        !> (window.h new_total, and new_pixel: windows are sized in whole
        !> lines and columns), applied by `window_resize_apply'.
        integer :: new_total = 0
        !> When the window was last selected (window.h use_time).
        integer(int64) :: use_time = 0
        !> The window ends before the frame's right edge, so its last column
        !> holds the vertical border (a terminal's window divider).
        logical :: divider = .false.
        !> The minibuffer window: no mode line.
        logical :: mini = .false.
        !> The height of the header line redisplay last drew (lines on a
        !> terminal, pixels on a window system; 0 for none): the text area
        !> starts below it (w->header_line_height).
        integer :: header_line_height = 0
        !> The height of the tab line redisplay last drew, above the header
        !> line (w->tab_line_height, as `header_line_height').
        integer :: tab_line_height = 0
        !> The window parameters of the tab line (`window_tab_line_type').
        type(window_tab_line_type) :: tab_line
        !> Where redisplay put the cursor, relative to the window; -1: none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> On a window-system frame, where and how large redisplay draws the
        !> cursor, in frame pixels (window.h phys_cursor with
        !> phys_cursor_width and phys_cursor_height).
        integer :: phys_cursor_x = 0, phys_cursor_y = 0, phys_cursor_width = 0, phys_cursor_height = 0
        !> Show no cursor in the window (`internal_show_cursor').
        logical :: cursor_off_p = .false.
        !> Columns scrolled off the left (only the minibuffer window
        !> scrolls horizontally so far: auto-hscroll).
        integer :: hscroll = 0
        !> The start was set explicitly; redisplay must keep it.
        logical :: force_start = .false.
        !> Where the last display ended, counted back from the end of the
        !> buffer text (window.h window_end_pos: Z less the position after
        !> the last character shown), and whether it reached the end of the
        !> buffer (for "Bot"/"All" in the mode line).
        integer(int64) :: window_end_pos = 0
        logical :: window_end_valid = .false.
        !> A deleted window kept because a window configuration refers to
        !> it: `set_window_configuration' may revive it.  (Upstream keeps
        !> deleted windows until garbage collection.)
        logical :: dead = .false.
        !> How many window configurations refer to the window.
        integer :: config_refs = 0
        !> The buffer a window shown in a window configuration showed when
        !> it was taken out of the tree (window.c keeps it in
        !> combination_limit for resurrection).
        type(handle_type) :: kept_buffer
        !> What `execute_window_change_functions' last recorded of the
        !> window: its buffer, its frame's change stamp then, and its total
        !> and body sizes (window.h old_buffer, change_stamp,
        !> old_pixel_height, old_pixel_width, old_body_pixel_height and
        !> old_body_pixel_width, in lines and columns).
        type(handle_type) :: old_buffer
        integer :: change_stamp = 0
        integer :: old_total_lines = 0, old_total_cols = 0, old_body_lines = 0, old_body_cols = 0
    end type window_type

    !> One window of a window configuration.  Port of window.c:struct
    !> saved_window (the slots used on a terminal).
    type :: saved_window_type
        type(handle_type) :: window, buffer
        integer :: left_col = 0, top_line = 0, total_cols = 0, total_lines = 0
        integer :: hscroll = 0, dedicated = DEDICATED_NONE
        logical :: divider = .false.
        real(real64) :: normal_lines = 1.0_real64, normal_cols = 1.0_real64
        !> Indices of the parent and previous sibling in the saved
        !> windows (0 for none).
        integer :: parent = 0, prev = 0
        !> Copies of the window's start and point (null for an internal
        !> window).
        type(handle_type) :: start, pointm
        !> The window's persistent parameters (window-persistent-parameters
        !> with the value t): `tab_line_buffers'.
        type(handle_type), allocatable :: tab_line_buffers(:)
    end type saved_window_type

    !> A window configuration: the windows of the frame, their sizes and
    !> buffers, where display starts and point in each.  Free it with
    !> `destroy_window_configuration'.  Port of window.c:struct
    !> save_window_data.
    type, public :: window_configuration_type
        type(saved_window_type), allocatable :: saved(:)
        type(handle_type) :: root_window, current_window, current_buffer, minibuf_scroll_window
        !> The frame's size then, in columns and lines (frame_cols,
        !> frame_lines).
        integer :: frame_cols = 0, frame_lines = 0
    end type window_configuration_type

    !> A window of a window state: its place in the tree (the indices of
    !> its parent and previous sibling in the state, 0 for none), its
    !> edges relative to the state's root window, its sizes, and for a live
    !> window its buffer with copies of its start and point, its other
    !> buffer settings, its previous and next buffers and its persistent
    !> parameters.  Not a port.
    type :: window_state_window_type
        integer :: parent = 0, prev = 0
        logical :: live = .false., horizontal = .false., divider = .false.
        integer :: left_col = 0, top_line = 0, total_cols = 0, total_lines = 0
        real(real64) :: normal_lines = 1.0_real64, normal_cols = 1.0_real64
        type(handle_type) :: buffer, start, point
        logical :: selected = .false.
        integer :: hscroll = 0, dedicated = DEDICATED_NONE
        type(prev_buffer_type), allocatable :: prev_buffers(:)
        type(handle_type), allocatable :: next_buffers(:)
        !> The persistent parameter `tab_line_buffers'.
        type(handle_type), allocatable :: tab_line_buffers(:)
    end type window_state_window_type

    !> The state of a window and its subwindows, depth first, which
    !> `set_window_state' puts back as new windows.  Free it with
    !> `destroy_window_state'.  Port of window.el's window state (as
    !> window-state-get makes it with WRITABLE false: markers, not
    !> positions; no window parameters besides the persistent ones, pixel
    !> sizes or minimum sizes).
    type, public :: window_state_type
        type(window_state_window_type), allocatable :: windows(:)
    end type window_state_type

    !> The window registry, indexed by handle (windows are objects with
    !> explicit lifetimes: make_window, delete_window_object).
    type(window_type), allocatable, target, public :: windows(:)
    type(slot_table_type) :: wtable

    ! Scrolling and recentering, in submodule window_scroll_mod: they
    ! move by screen lines (indent_mod), which depends on this module.
    interface
        !> Scroll text of selected window upward ARG lines.
        !> If ARG is PREFIX_NONE (no prefix argument), scroll upward by a near full screen.
        !> A near full screen is `next_screen_context_lines' less than a
        !> full screen.  Negative ARG means scroll downward.  If ARG is
        !> PREFIX_MINUS, scroll downward by nearly full screen.  When calling
        !> from a program, supply as argument a number (PREFIX_NUMBER),
        !> PREFIX_NONE or PREFIX_MINUS.
        !> Port of window.c:Fscroll_up.
        module subroutine scroll_up(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up

        !> Scroll text of selected window down ARG lines.
        !> If ARG is PREFIX_NONE (no prefix argument), scroll down by a near full screen.
        !> A near full screen is `next_screen_context_lines' less than a
        !> full screen.  Negative ARG means scroll upward.  If ARG is
        !> PREFIX_MINUS, scroll upward by nearly full screen.  When calling
        !> from a program, supply as argument a number (PREFIX_NUMBER),
        !> PREFIX_NONE or PREFIX_MINUS.
        !> Port of window.c:Fscroll_down.
        module subroutine scroll_down(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down

        !> Center point in selected window and maybe redisplay frame.
        !> With a numeric prefix argument ARG, recenter putting point on
        !> screen line ARG relative to the selected window.  If ARG is
        !> negative, it counts up from the bottom of the window.  (ARG
        !> should be less than the height of the window.)
        !>
        !> If ARG is PREFIX_NONE, then recenter with point on the middle
        !> line of the selected window; if REDISPLAY is true, also erase the
        !> entire frame and redraw it.  Interactively, REDISPLAY is always
        !> true.
        !>
        !> Just C-u as prefix means put point in the center of the window
        !> and redisplay normally--don't erase and redraw the frame.
        !> Port of window.c:Frecenter (`recenter_redisplay' is `tty').
        module subroutine recenter(arg, redisplay, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            logical, intent(in) :: redisplay
            type(status_type), intent(out) :: status
        end subroutine recenter

        !> Position point relative to window.
        !> ARG PREFIX_NONE means position point at center of window.
        !> Else, ARG specifies vertical position within the window;
        !> zero means top of window, negative means relative to bottom
        !> of window, -1 meaning the last fully visible display line
        !> of the window.
        !>
        !> `line' is the screen line of the window point moved to, counting
        !> from the top of the window.  Port of window.c:Fmove_to_window_line (with
        !> window.c:displayed_window_lines, on a terminal the body height).
        module subroutine move_to_window_line(arg, line, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            integer(int64), intent(out) :: line
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line

        !> Scroll text of selected window upward ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll up by that many lines (and down by that many
        !> lines if the number is negative).  Without a prefix, scroll up by
        !> a full screen.  If given a `C-u -' prefix, scroll a full page
        !> down instead.
        !>
        !> If `scroll_error_top_bottom' is true and `scroll_up' cannot
        !> scroll window further, move cursor to the bottom line.
        !> When point is already on that position, then an error condition occurs.
        !> A near full screen is `next_screen_context_lines' less than a
        !> full screen.  Negative ARG means scroll downward.
        !>
        !> If ARG is PREFIX_MINUS, scroll downward by nearly full screen.
        !>
        !> The command \[set_goal_column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-up-command.
        recursive module subroutine scroll_up_command(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up_command

        !> Scroll text of selected window down ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll down by that many lines (and up by that many
        !> lines if the number is negative).  Without a prefix, scroll down
        !> by a full screen.  If given a `C-u -' prefix, scroll a full page
        !> up instead.
        !>
        !> If `scroll_error_top_bottom' is true and `scroll_down' cannot
        !> scroll window further, move cursor to the top line.
        !> When point is already on that position, then an error condition occurs.
        !> A near full screen is `next_screen_context_lines' less than a
        !> full screen.  Negative ARG means scroll upward.
        !>
        !> If ARG is PREFIX_MINUS, scroll upward by nearly full screen.
        !>
        !> The command \[set_goal_column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-down-command.
        recursive module subroutine scroll_down_command(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down_command

        !> Whether position POS is shown, at least partially, in WINDOW:
        !> within the window's body lines from its start.  Port of
        !> window.c:Fpos_visible_in_window_p with PARTIALLY .true. (line
        !> based, no vscroll).
        module logical function is_pos_visible_in_window(pos, window) result(visible)
            implicit none(type, external)
            integer(int64), intent(in) :: pos
            type(handle_type), intent(in) :: window
        end function is_pos_visible_in_window

        !> Return position at which display currently ends in WINDOW.
        !> This is the position after the final character in WINDOW.
        !>
        !> WINDOW must be a live window.  This is updated by redisplay,
        !> when it runs to completion.  Simply changing the buffer text or
        !> setting `window_start' does not update this value.
        !>
        !> If UPDATE is true, compute the up-to-date position if it isn't
        !> already recorded.  Port of window.c:Fwindow_end (with UPDATE, the
        !> position is always computed: redisplay keeps no record of what
        !> changed since, window_outdated).
        module function window_end(window, status, update) result(pos)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: update
            integer(int64) :: pos
        end function window_end

        !> Run `window_scroll_functions' for WINDOW.
        !> This function is called by `split_window' for the new window, after it
        !> has established the size of the new window.  Port of
        !> window.c:Fexecute_window_scroll_functions.
        recursive module subroutine execute_window_scroll_functions(window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
        end subroutine execute_window_scroll_functions

        !> Return "the other" window for "other window scroll" commands: a
        !> neighboring window on the selected frame.  Port of
        !> window.c:Fother_window_for_scrolling (`minibuffer_scroll_window',
        !> `other_window_scroll_buffer' and `other_window_scroll_default'
        !> are deferred: they are unset).
        module subroutine other_window_for_scrolling(window, status)
            implicit none(type, external)
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
        end subroutine other_window_for_scrolling

        !> Scroll next window upward LINES lines; or near full screen if no
        !> ARG.  See `scroll_up_command' for details.  Port of
        !> window.el:scroll-other-window.
        recursive module subroutine scroll_other_window(lines, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: lines
            type(status_type), intent(out) :: status
        end subroutine scroll_other_window

        !> Scroll next window downward LINES lines; or near full screen if
        !> no ARG.  See `scroll_down_command' for details.  Port of
        !> window.el:scroll-other-window-down.
        recursive module subroutine scroll_other_window_down(lines, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: lines
            type(status_type), intent(out) :: status
        end subroutine scroll_other_window_down

        !> Scroll the window so that current line is in the middle of the
        !> window.  Successive invocations scroll the window in a cyclical
        !> order to put the current line at certain places within the
        !> window, as determined by `recenter_positions'.  By default, the
        !> second invocation puts the current line at the top-most window
        !> line, the third invocation puts it on the bottom-most window
        !> line, and then the order is reused in a cyclical manner.
        !>
        !> With numeric prefix ARG, move current line ARG lines below the
        !> window top.  With plain \[universal_argument], move current line
        !> to window center.  Port of window.el:recenter-top-bottom.
        module subroutine recenter_top_bottom(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine recenter_top_bottom

        !> Position point relative to window.
        !>
        !> With a prefix argument ARG, acts like `move_to_window_line'.
        !>
        !> With no argument, positions point at center of window.
        !> Successive calls position point at positions defined
        !> by `recenter_positions'.  Port of
        !> window.el:move-to-window-line-top-bottom.
        module subroutine move_to_window_line_top_bottom(arg, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line_top_bottom

        !> Register the scrolling commands and `quit_window'.  Not a port.
        module subroutine init_window_commands()
            implicit none(type, external)
        end subroutine init_window_commands

        ! The window tree and buffer display, in submodule window_commands_mod.

        !> Select WINDOW which must be a live window.  Also make WINDOW's
        !> buffer current and make WINDOW the frame's selected window; point
        !> becomes WINDOW's point.  Unless `norecord', record the buffer as
        !> most recently selected.  Port of window.c:Fselect_window, with
        !> window.c:select_window, window.c:select_window_1 and, for
        !> NORECORD, window.c:select_window_norecord.
        recursive module subroutine select_window(h, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine select_window

        !> The minimum size of WINDOW, in lines or with HORIZONTAL in
        !> columns: for a live window `window_min_height' or
        !> `window_min_width' (at least one text line and its mode line, or
        !> two columns), for an internal window the sum of its children's
        !> along its combination or else their maximum.  IGNORE true means
        !> ignore `window_min_height' and `window_min_width': a live window
        !> then needs its safe minimum and its mode line.  Port of
        !> window.el:window-min-size (no fixed-size or preserved windows;
        !> IGNORE .true. is upstream's t).
        recursive module integer function window_min_size(window, horizontal, ignore) result(n)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            logical, intent(in) :: horizontal
            logical, intent(in), optional :: ignore
        end function window_min_size

        !> Make a new window adjacent to WINDOW: below it, or with
        !> `horizontal' to its right.  Without `has_size' both get half its
        !> size (or close to it); a nonnegative SIZE is the new size of
        !> WINDOW, a negative one minus the size of the new window, in lines
        !> or columns.  The new window shows the buffer, start and point of
        !> WINDOW if it is live, else of the selected window.  WINDOW may
        !> be internal (the root window, say): its windows share what is
        !> left.  With `before' the new window goes above WINDOW (or to its
        !> left).  `side_resize' stands for `window_combination_resize'
        !> bound to `side', as `display_buffer_in_side_window' does: the
        !> new window takes space from the side windows next to it and
        !> becomes a side window.  Port of window.el:split-window
        !> (`window_combination_resize' .false. otherwise, no REFER).
        module subroutine split_window(window, size, has_size, horizontal, new, status, before, side_resize)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: size
            logical, intent(in) :: has_size, horizontal
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: before, side_resize
        end subroutine split_window

        !> Split the selected window into two windows, one above the other,
        !> both of the same height (or close to it); NEW is the new window,
        !> below.  Port of window.el:split-window-below (no SIZE; the
        !> function, as `ediff_split_window_function' calls it).
        module subroutine split_window_below(new, status)
            implicit none(type, external)
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine split_window_below

        !> Split the selected window into two side-by-side windows of the
        !> same width (or close to it); NEW is the new window, to the right.
        !> Port of window.el:split-window-right (no SIZE; the function, as
        !> `ediff_split_window_function' calls it).
        module subroutine split_window_right(new, status)
            implicit none(type, external)
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine split_window_right

        !> Delete WINDOW: its space goes to a neighbor, and if it was
        !> selected the most recently used window is selected.  Port of
        !> window.el:delete-window.
        module subroutine delete_window(window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
        end subroutine delete_window

        !> Make WINDOW fill its frame, or with side windows its main window
        !> (side windows and windows with `no_delete_other_windows' stay).
        !> INTERACTIVE says "No other windows to delete" when there is
        !> nothing to do.  Port of window.el:delete-other-windows.
        module subroutine delete_other_windows(window, status, interactive)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: interactive
        end subroutine delete_other_windows

        !> Select another window in cyclic ordering of windows.
        !> COUNT specifies the number of windows to skip, starting with the
        !> selected window, before making the selection.  If COUNT is
        !> positive, skip COUNT windows forwards.  If COUNT is negative,
        !> skip -COUNT windows backwards.  Windows whose `no_other_window' parameter is
        !> true are skipped.  INTERACTIVE says "No other window to select"
        !> when it selects the selected window.  Port of window.el:other-window.
        module subroutine other_window(count, status, interactive)
            implicit none(type, external)
            integer(int64), intent(in) :: count
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: interactive
        end subroutine other_window

        !> Return most recently selected buffer other than BUFFER.
        !> Buffers not visible in windows are preferred to visible buffers.
        !> Ignore internal buffers (names starting with a space).  If no
        !> other buffer exists, return the buffer `*scratch*' (creating it
        !> if necessary).  If VISIBLE_OK is true, visible buffers are not
        !> passed over.  Port of buffer.c:Fother_buffer.
        module function other_buffer(buffer, visible_ok) result(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            logical, intent(in), optional :: visible_ok
            type(handle_type) :: h
        end function other_buffer

        !> Display buffer NAME in the selected window, making it current; a
        !> name of no live buffer makes a new buffer of that name.  If the
        !> selected window cannot show it (it is the minibuffer window or
        !> strongly dedicated), use `pop_to_buffer', or with
        !> FORCE_SAME_WINDOW an error condition occurs.  Unless NORECORD, the buffer
        !> becomes the most recently selected one.  When the window showed
        !> the buffer before, its start and point there are restored
        !> (`switch_to_buffer_preserve_window_point' .true.).  Port of
        !> window.el:switch-to-buffer (`switch_to_buffer_obey_display_actions'
        !> and the other values of `switch_to_buffer_preserve_window_point'
        !> are deferred).
        module subroutine switch_to_buffer(name, status, norecord, force_same_window)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord, force_same_window
        end subroutine switch_to_buffer

        !> In selected window switch to ARGth previous buffer (absent: the
        !> first).  Call `switch_to_prev_buffer' unless the selected window
        !> is the minibuffer window or is dedicated to its buffer.  With
        !> INTERACTIVE true, a `user_error' error condition occurs when
        !> there is no previous buffer.  Port of window.el:previous-buffer.
        module subroutine previous_buffer(status, arg, interactive)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
            logical, intent(in), optional :: interactive
        end subroutine previous_buffer

        !> In selected window switch to ARGth next buffer (absent: the
        !> first).  Call `switch_to_next_buffer' unless the selected window
        !> is the minibuffer window or is dedicated to its buffer.  With
        !> INTERACTIVE true, a `user_error' error condition occurs when
        !> there is no next buffer.  Port of window.el:next-buffer.
        module subroutine next_buffer(status, arg, interactive)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
            logical, intent(in), optional :: interactive
        end subroutine next_buffer

        !> Register the window and buffer commands.  Not a port.
        module subroutine init_window_cmds()
            implicit none(type, external)
        end subroutine init_window_cmds
        ! Showing buffers in windows, in submodule window_display_mod.

        !> Make WINDOW display BUFFER_OR_NAME.
        !> WINDOW must be a live window.  Optional third argument
        !> KEEP-MARGINS .true. means that WINDOW's current display margins,
        !> fringe widths, and scroll bar settings are preserved.  This
        !> an error condition occurs when WINDOW is strongly dedicated to its
        !> buffer and does not already display BUFFER_OR_NAME.  Port of
        !> window.c:Fset_window_buffer.
        module subroutine set_window_buffer(h, buffer, status, keep_margins)
            implicit none(type, external)
            type(handle_type), intent(in) :: h, buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: keep_margins
        end subroutine set_window_buffer

        !> Port of window.c:set_window_buffer.
        module subroutine set_window_buffer_internal(h, buffer, keep_margins)
            implicit none(type, external)
            type(handle_type), intent(in) :: h, buffer
            logical, intent(in) :: keep_margins
        end subroutine set_window_buffer_internal

        !> Port of window.c:window_discard_buffer_from_window.
        module subroutine window_discard_buffer_from_window(buffer, h, all)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer, h
            logical, intent(in) :: all
        end subroutine window_discard_buffer_from_window

        !> Port of window.el:record-window-buffer.
        module subroutine record_window_buffer(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
        end subroutine record_window_buffer

        !> Port of window.el:switch-to-prev-buffer.
        module function switch_to_prev_buffer(window, bury_or_close, status) result(new_buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: bury_or_close
            type(status_type), intent(out) :: status
            type(handle_type) :: new_buffer
        end function switch_to_prev_buffer

        !> Port of window.el:switch-to-next-buffer.
        module function switch_to_next_buffer(window, status) result(new_buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: new_buffer
        end function switch_to_next_buffer

        !> Port of window.el:quit-restore-window.
        module subroutine quit_restore_window(window, bury_or_close, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: bury_or_close
            type(status_type), intent(out) :: status
        end subroutine quit_restore_window

        !> Quit all windows showing BUFFER.
        !> KILL true means to kill BUFFER.  KILL false means to bury
        !> BUFFER.
        !>
        !> This function calls `quit_restore_window' on all candidate windows
        !> showing BUFFER.  In addition, it removes the buffer
        !> from all window-local buffer lists.  Port of
        !> window.el:quit-windows-on (the selected frame; no
        !> `quit_window_hook').  (In window_display_mod.)
        module subroutine quit_windows_on(buffer, kill, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: kill
            type(status_type), intent(out) :: status
        end subroutine quit_windows_on

        !> Quit WINDOW (the selected one by default) and bury its buffer, or
        !> with KILL kill it.  Port of window.el:quit-window.
        module subroutine quit_window(kill, status, window)
            implicit none(type, external)
            logical, intent(in) :: kill
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: window
        end subroutine quit_window

        !> Port of window.el:replace-buffer-in-windows (and of
        !> window.c:replace_buffer_in_windows, which calls it).
        module subroutine replace_buffer_in_windows(buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
        end subroutine replace_buffer_in_windows

        !> Put BUFFER (the current buffer by default) at the end of the
        !> list of all buffers.  Port of window.el:bury-buffer.
        module subroutine bury_buffer(status, buffer)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: buffer
        end subroutine bury_buffer

        !> Port of window.el:display-buffer-record-window.
        module subroutine display_buffer_record_window(type, h, buffer)
            implicit none(type, external)
            integer, intent(in) :: type
            type(handle_type), intent(in) :: h, buffer
        end subroutine display_buffer_record_window

        !> Display BUFFER in some window and return that window (the null
        !> handle if none could be found): the action functions ACTIONS
        !> (DISPLAY_ACTION_* codes) are tried in turn with TABLE, after
        !> ACTION_FUNCTION if present (an action function of the ACTION
        !> argument), then those of `display_buffer_fallback_action'.  Port of
        !> window.el:display-buffer (and of window.c:display_buffer, which
        !> calls it).
        module subroutine display_buffer(buffer, window, status, actions, table, action_function)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: actions(:)
            type(display_action_parameters_type), intent(in), optional :: table
            procedure(display_action_function_procedure), optional :: action_function
        end subroutine display_buffer

        !> Display BUFFER in no window.
        !> If TABLE contains a true `allow_no_window' entry, do nothing
        !> and give FAILED true.  This allows `display_buffer' to override the
        !> default action and avoid displaying the buffer.  It is assumed
        !> that when the caller specifies a true `allow_no_window' table
        !> entry, it can handle a null window given by `display_buffer'.
        !>
        !> This is an action function for buffer display (a
        !> `display_action_type''s `action_function').  It should be
        !> called only by `display_buffer' or a procedure directly or
        !> indirectly called by the latter.  WINDOW is always the null
        !> handle.  Port of window.el:display-buffer-no-window.
        recursive module subroutine display_buffer_no_window(buffer, table, window, failed, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(display_action_parameters_type), intent(in) :: table
            type(handle_type), intent(out) :: window
            logical, intent(out) :: failed
            type(status_type), intent(out) :: status
        end subroutine display_buffer_no_window

        !> Display BUFFER and select its window.  Port of
        !> window.el:pop-to-buffer.
        module subroutine pop_to_buffer(buffer, status, actions, table, norecord, action_function)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: actions(:)
            type(display_action_parameters_type), intent(in), optional :: table
            logical, intent(in), optional :: norecord
            procedure(display_action_function_procedure), optional :: action_function
        end subroutine pop_to_buffer

        !> Select the buffer BUFFER in another window.  Return the buffer
        !> switched to.  Unless NORECORD, BUFFER becomes the most
        !> recently selected buffer.
        !>
        !> This uses the function `display_buffer' as a subroutine; see its
        !> documentation for additional customization information.
        !> If this command needs to split the current window, it by default
        !> obeys the user options `split_height_threshold' and
        !> `split_width_threshold', when it decides whether to split the
        !> window horizontally or vertically.  Port of
        !> window.el:switch-to-buffer-other-window (a buffer, not a name).
        module subroutine switch_to_buffer_other_window(buffer, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine switch_to_buffer_other_window

        !> Switch to buffer BUFFER in another frame.  Return the buffer
        !> switched to.
        !>
        !> This uses the function `display_buffer' as a subroutine to
        !> display the buffer; see its documentation for additional
        !> customization information.  By default, if the buffer is
        !> already displayed (even in the current frame), that window is
        !> selected.  If the buffer isn't displayed in any frame, a new
        !> frame is popped up and the buffer is displayed there.
        !>
        !> Unless NORECORD, BUFFER becomes the most recently selected
        !> buffer.  Port of window.el:switch-to-buffer-other-frame (a
        !> buffer, not a name).
        module subroutine switch_to_buffer_other_frame(buffer, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine switch_to_buffer_other_frame

        !> Display buffer BUFFER preferably in another frame.
        !> This function attempts to look for a window displaying BUFFER,
        !> on all the frames on the current terminal, skipping the selected
        !> window; if that fails, it pops up a new frame.
        !> This uses the function `display_buffer' as a subroutine; see
        !> its documentation for additional customization information.
        !> Port of window.el:display-buffer-other-frame.
        module subroutine display_buffer_other_frame(buffer, window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
        end subroutine display_buffer_other_frame

        !> Return window in DIRECTION as seen from WINDOW.
        !> More precisely, return the nearest window in direction DIRECTION
        !> as seen from the position of `window_point' in window WINDOW.
        !> DIRECTION should be one of DIRECTION_ABOVE, DIRECTION_BELOW,
        !> DIRECTION_LEFT or DIRECTION_RIGHT.  WINDOW must be a live window
        !> and defaults to the selected one.
        !>
        !> Optional argument SIGN a negative number means to use the right
        !> or bottom edge of WINDOW as reference position instead of
        !> `window_point'.  SIGN a positive number means to use the left or
        !> top edge of WINDOW as reference position.
        !>
        !> Optional argument WRAP true means to wrap DIRECTION around
        !> frame borders.  This means to return for WINDOW at the top of the
        !> frame and DIRECTION DIRECTION_ABOVE the minibuffer window if the frame
        !> has one, and a window at the bottom of the frame otherwise.
        !>
        !> Optional argument MINIBUF MINIBUF_YES means to return the minibuffer
        !> window even if it isn't active.  MINIBUF MINIBUF_ACTIVE or omitted means
        !> to return the minibuffer window if and only if it is currently active.
        !> MINIBUF MINIBUF_NO means never return the minibuffer window.
        !>
        !> Return the null handle if no suitable window can be found.  Port of
        !> window.el:window-in-direction (with window.el:window--in-direction-2
        !> and window.el:walk-window-tree; IGNORE makes no difference and is
        !> left out, since no window has a `no_other_window' parameter yet).
        module function window_in_direction(direction, window, sign, wrap, minibuf) result(best)
            implicit none(type, external)
            integer, intent(in) :: direction
            type(handle_type), intent(in), optional :: window
            integer(int64), intent(in), optional :: sign
            logical, intent(in), optional :: wrap
            integer, intent(in), optional :: minibuf
            type(handle_type) :: best
        end function window_in_direction

        !> Port of window.el:pop-to-buffer-same-window.
        module subroutine pop_to_buffer_same_window(buffer, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine pop_to_buffer_same_window

        !> Adjust the height of WINDOW to display its buffer's contents
        !> exactly, but at most MAX_HEIGHT lines (by default as many as
        !> the other windows can give) and at least MIN_HEIGHT lines (by
        !> default `window_min_height').
        !> Only a window in a vertical combination is resized.  Port of
        !> window.el:fit-window-to-buffer (vertically; PRESERVE-SIZE and
        !> the width arguments are deferred).
        module subroutine fit_window_to_buffer(window, max_height, min_height)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in), optional :: max_height, min_height
        end subroutine fit_window_to_buffer

        !> Shrink height of WINDOW if its buffer doesn't need so many lines.
        !> More precisely, shrink WINDOW vertically to be as small as
        !> possible, while still showing the full contents of its buffer.
        !> WINDOW must be a live window and defaults to the selected one.
        !>
        !> Do not shrink WINDOW to less than `window_min_height' lines.  Do
        !> nothing if the buffer contains more lines than the present window
        !> height, or if some of the window's contents are scrolled out of
        !> view, or if shrinking this window would also shrink another
        !> window, or if the window is the only window of its frame.
        !> Port of window.el:shrink-window-if-larger-than-buffer.
        !> (In window_display_mod.)
        module subroutine shrink_window_if_larger_than_buffer(window)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
        end subroutine shrink_window_if_larger_than_buffer

        !> Show temporary buffer BUFFER in a window: unmodified, read-only
        !> and at its beginning, displayed with ACTIONS and TABLE.  WINDOW
        !> is the window showing BUFFER; it becomes
        !> `minibuffer_scroll_window'.  Port of
        !> window.el:temp-buffer-window-show (no `temp_buffer_resize_mode').
        module subroutine temp_buffer_window_show(buffer, window, status, actions, table)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: actions(:)
            type(display_action_parameters_type), intent(in), optional :: table
        end subroutine temp_buffer_window_show

        !> Scroll WINDOW up (DIRECTION 1) or down (-1) by nearly a full
        !> screen, as `scroll_up' and `scroll_down' with no argument do in
        !> the selected window; WINDOW's point moves as needed.  (Upstream
        !> runs them with WINDOW selected: with-selected-window.)
        !> Not a port.
        module subroutine scroll_window(window, direction, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: direction
            type(status_type), intent(out) :: status
        end subroutine scroll_window

        !> Port of window.c:Fget_buffer_window (with
        !> window.c:window_loop for GET_BUFFER_WINDOW).
        module function get_buffer_window(buffer) result(window)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type) :: window
        end function get_buffer_window

        !> Port of window.el:get-lru-window.
        module function get_lru_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_lru_window

        !> Port of window.el:get-mru-window.
        module function get_mru_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_mru_window

        !> Port of window.el:get-largest-window.
        module function get_largest_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_largest_window

        !> Whether `h' is combined with siblings above and below (or with
        !> HORIZONTAL, side by side).  Port of window.el:window-combined-p.
        logical module function is_window_combined(h, horizontal)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            logical, intent(in) :: horizontal
        end function is_window_combined

        !> Return true if WINDOW is as high as its containing frame.
        !> More precisely, return true if and only if the total height of
        !> WINDOW equals the total height of the root window of WINDOW's
        !> frame.  Port of window.el:window-full-height-p.
        logical module function is_window_full_height(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
        end function is_window_full_height

        !> Return true if `split_window_sensibly' may split WINDOW: one at
        !> least `split_height_threshold' lines high and twice
        !> `window_min_height' (or 2), or with HORIZONTAL at least
        !> `split_width_threshold' columns wide and twice `window_min_width'
        !> (or 2).  THRESHOLD_ZERO stands for `split_height_threshold' bound
        !> to 0.  Port of window.el:window-splittable-p.
        logical module function is_window_splittable(h, horizontal, threshold_zero)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            logical, intent(in) :: horizontal
            logical, intent(in), optional :: threshold_zero
        end function is_window_splittable

        !> Split a window as split-window-below or split-window-right do.
        !> Port of window.el:split-window-below and
        !> window.el:split-window-right, in part.
        module subroutine split_window_and_copy(h, horizontal, new, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            logical, intent(in) :: horizontal
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine split_window_and_copy

        !> "#<window N on BUFFER>" for window `h' (how prin1 prints it).
        !> Not a port.
        module function window_description(h) result(s)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            character(len=:), allocatable :: s
        end function window_description

        !> Copy a `quit_restore' parameter.  Not a port.
        module subroutine copy_quit_restore(from, to)
            implicit none(type, external)
            type(quit_restore_type), intent(in) :: from
            type(quit_restore_type), intent(inout) :: to
        end subroutine copy_quit_restore

        !> Resize WINDOW vertically by DELTA lines, or with HORIZONTAL
        !> horizontally by DELTA columns: the window next to it (or, if it
        !> is not combined in that direction, next to the combination it is
        !> in) gives or takes the lines.  Port of window.el:window-resize
        !> (taking from further siblings, which upstream does in proportion
        !> to their normal sizes, is deferred: the error condition "Cannot resize
        !> window" occurs).
        module subroutine window_resize(window, delta, horizontal, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: delta
            logical, intent(in) :: horizontal
            type(status_type), intent(out) :: status
        end subroutine window_resize

        !> Return an object representing the current window configuration
        !> of the selected frame.  This describes the number of windows,
        !> their sizes and current buffers, and for each displayed buffer,
        !> where display starts, and the position of point.  An exception
        !> is made for point in the current buffer: its value is -not-
        !> saved.  Free it with `destroy_window_configuration'.  Port of
        !> window.c:Fcurrent_window_configuration (one frame; no persistent
        !> window parameters).
        module subroutine current_window_configuration(config)
            implicit none(type, external)
            type(window_configuration_type), intent(out) :: config
        end subroutine current_window_configuration

        !> Set the configuration of windows and buffers as specified by
        !> CONFIGURATION, a value previously made by
        !> `current_window_configuration'.  The minibuffer window keeps its
        !> buffer.  Port of window.c:Fset_window_configuration (and
        !> window.c:restore_window_configuration) with
        !> DONT-SET-FRAME and DONT-SET-MINIWINDOW .true.
        !> (`window_restore_closed_buffer_windows' .false.).
        module subroutine set_window_configuration(config)
            implicit none(type, external)
            type(window_configuration_type), intent(in) :: config
        end subroutine set_window_configuration

        !> Free window configuration CONFIG: its markers, and the deleted
        !> windows only it kept.  Not a port (upstream relies on GC).
        module subroutine destroy_window_configuration(config)
            implicit none(type, external)
            type(window_configuration_type), intent(inout) :: config
        end subroutine destroy_window_configuration

        !> Make COPY a window configuration of its own equal to CONFIG: new
        !> markers for its starts and points, and the windows it keeps
        !> counted once more.  Free it with `destroy_window_configuration'.
        !> Not a port (upstream shares the Lisp object).
        module subroutine copy_window_configuration(config, copy)
            implicit none(type, external)
            type(window_configuration_type), intent(in) :: config
            type(window_configuration_type), intent(out) :: copy
        end subroutine copy_window_configuration

        !> Return state of the selected frame's root window and its
        !> subwindows: their sizes and structure, and for each live window
        !> its buffer, where display starts, point, horizontal scrolling,
        !> dedication and previous and next buffers.  Free it with
        !> `destroy_window_state'.  Port of window.el:window-state-get
        !> (WINDOW the frame's root window, WRITABLE false; window
        !> parameters, pixel and minimum sizes are not recorded).
        module subroutine get_window_state(state)
            implicit none(type, external)
            type(window_state_type), intent(out) :: state
        end subroutine get_window_state

        !> Put window state STATE into the selected frame: its windows are
        !> deleted and new ones made with STATE's structure, sizes, buffers,
        !> starts and points; the window that was selected when STATE was
        !> made is selected.  STATE's root window must be as large as the
        !> frame's (else an `error' error condition occurs).  Port of
        !> window.el:window-state-put (WINDOW absent, the totals equal; resizing
        !> to other sizes and window parameters are Deferred).
        module subroutine set_window_state(state, status)
            implicit none(type, external)
            type(window_state_type), intent(in) :: state
            type(status_type), intent(out) :: status
        end subroutine set_window_state

        !> Free window state STATE: its markers.  Not a port (upstream
        !> relies on GC).
        module subroutine destroy_window_state(state)
            implicit none(type, external)
            type(window_state_type), intent(inout) :: state
        end subroutine destroy_window_state

        !> Say whether two window configurations have the same window layout.
        !> This function ignores details such as the values of point and
        !> scrolling positions.  Port of window.c:Fwindow_configuration_equal_p
        !> (with window.c:compare_window_configurations).
        module function is_window_configuration_equal(x, y) result(equal)
            implicit none(type, external)
            type(window_configuration_type), intent(in) :: x, y
            logical :: equal
        end function is_window_configuration_equal

        !> Run `window_configuration_change_hook' for the selected frame.
        !> Its buffer-local values run with the window showing the buffer
        !> selected, the default value with the frame selected; all with the
        !> selected window's buffer current.  STATUS is that of the first
        !> function that fails, which stops the hook.  Port of
        !> window.c:Frun_window_configuration_change_hook (with
        !> window.c:run_window_configuration_change_hook and window.c:run_funs;
        !> one frame).
        recursive module subroutine execute_window_configuration_change_hook(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_window_configuration_change_hook

        !> Run the window change hooks of the selected frame if a window
        !> changed its buffer or size, or one was deleted, since the last
        !> time (redisplay calls it): `window_configuration_change_hook'.
        !> Then record the windows' buffers and sizes for the next time.
        !> Port of window.c:run_window_change_functions (with
        !> window.c:window_change_record and
        !> window.c:window_change_record_windows; one frame; the other
        !> hooks, `window_buffer_change_functions',
        !> `window_size_change_functions', `window_selection_change_functions',
        !> `window_state_change_functions' and `window_state_change_hook',
        !> are Deferred).
        recursive module subroutine execute_window_change_functions(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_window_change_functions

        !> Prepare window `h' for no longer showing its buffer: remember
        !> where its display started and, unless another window shows the
        !> buffer, put the buffer's point where the window's point was.
        !> Port of window.c:unshow_buffer.
        module subroutine unshow_buffer(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
        end subroutine unshow_buffer

        !> Register `quit_window', `bury_buffer', `previous_buffer' and
        !> `next_buffer'.  Not a port.
        module subroutine init_window_display()
            implicit none(type, external)
        end subroutine init_window_display

        ! What windows are, in submodule window_info_mod.

        !> Return .true. if OBJECT is a window and .false. otherwise.  Port of
        !> window.c:Fwindowp (a handle `make_window' gave, deleted since or
        !> not).
        module function is_window(object) result(p)
            implicit none(type, external)
            type(handle_type), intent(in) :: object
            logical :: p
        end function is_window

        !> Return the buffer displayed in window WINDOW.
        !> If WINDOW is absent, it defaults to the selected window.
        !> Return the null handle for an internal window or a deleted window.  Port of
        !> window.c:Fwindow_buffer (with window.c:decode_any_window).
        module function window_buffer(window, status) result(b)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: b
        end function window_buffer

        !> Return the parent window of window WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Return the null handle for a window with no parent (e.g. a root window).
        !> Port of window.c:Fwindow_parent.
        module function window_parent(window, status) result(p)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: p
        end function window_parent

        !> Return the topmost child window of window WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Return the null handle if WINDOW is a live window (live windows have no children).
        !> Return the null handle if WINDOW is an internal window whose children form a
        !> horizontal combination.  Port of window.c:Fwindow_top_child.
        module function window_top_child(window, status) result(c)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: c
        end function window_top_child

        !> Return the leftmost child window of window WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Return the null handle if WINDOW is a live window (live windows have no children).
        !> Return the null handle if WINDOW is an internal window whose children form a
        !> vertical combination.  Port of window.c:Fwindow_left_child.
        module function window_left_child(window, status) result(c)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: c
        end function window_left_child

        !> Return the next sibling window of window WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Return the null handle if WINDOW has no next sibling.  Port of
        !> window.c:Fwindow_next_sibling.
        module function window_next_sibling(window, status) result(s)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: s
        end function window_next_sibling

        !> Return the previous sibling window of window WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Return the null handle if WINDOW has no previous sibling.  Port of
        !> window.c:Fwindow_prev_sibling.
        module function window_prev_sibling(window, status) result(s)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: s
        end function window_prev_sibling

        !> Return the root window of FRAME-OR-WINDOW.
        !> If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
        !> With a frame argument, return that frame's root window.
        !> With a window argument, return the root window of that window's frame.
        !> Port of window.c:Fframe_root_window (one frame: WINDOW, if given,
        !> must be valid).
        module function frame_root_window(window, status) result(r)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: r
        end function frame_root_window

        !> Return the selected window of FRAME-OR-WINDOW.
        !> If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
        !> Else if FRAME-OR-WINDOW denotes a valid window, return the selected
        !> window of that window's frame.  If FRAME-OR-WINDOW denotes a live frame,
        !> return the selected window of that frame.  Port of
        !> window.c:Fframe_selected_window (one frame).
        module function frame_selected_window(window, status) result(s)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: s
        end function frame_selected_window

        !> Return the minibuffer window for frame FRAME.
        !> If FRAME is absent, it defaults to the selected frame.
        !> Port of window.c:Fminibuffer_window (one frame).
        module function minibuffer_window() result(m)
            implicit none(type, external)
            type(handle_type) :: m
        end function minibuffer_window

        !> Return .true. if WINDOW is a minibuffer window.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Port of window.c:Fwindow_minibuffer_p.
        module function is_window_minibuffer(window, status) result(p)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            logical :: p
        end function is_window_minibuffer

        !> Return window selected just before minibuffer window was selected.
        !> Return the null handle if the selected window is not a minibuffer window.
        !> Port of window.c:Fminibuffer_selected_window.
        module function minibuffer_selected_window() result(s)
            implicit none(type, external)
            type(handle_type) :: s
        end function minibuffer_selected_window

        !> Return the height of window WINDOW in canonical lines.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> The return value includes the heights of WINDOW's mode and header line
        !> and its bottom divider, if any.  If WINDOW is an internal window, the
        !> total height is the height of the screen areas spanned by its children.
        !> Port of window.c:Fwindow_total_height (a window's height is a whole
        !> number of canonical lines, so ROUND makes no difference and is left
        !> out).
        module function window_total_height(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_total_height

        !> Return the total width of window WINDOW in canonical columns.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> The return value includes the widths of WINDOW's fringes, margins,
        !> scroll bars and its right divider, if any.  If WINDOW is an internal
        !> window, the total width is the width of the screen areas spanned by its
        !> children.  Port of window.c:Fwindow_total_width (ROUND left out, as
        !> for `window_total_height').
        module function window_total_width(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_total_width

        !> Return left column of window WINDOW.
        !> This is the distance, in columns, between the left edge of WINDOW and
        !> the left edge of the frame's window area.  For instance, the return
        !> value is 0 if there is no window to the left of WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Port of window.c:Fwindow_left_column.
        module function window_left_column(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_left_column

        !> Return top line of window WINDOW.
        !> This is the distance, in lines, between the top of WINDOW and the top
        !> of the frame's window area.  For instance, the return value is 0 if
        !> there is no window above WINDOW.
        !> WINDOW must be a valid window and defaults to the selected one.
        !> Port of window.c:Fwindow_top_line.
        module function window_top_line(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_top_line

        !> Return the height of WINDOW's text area.
        !> WINDOW must be a live window and defaults to the selected one.  The
        !> return value does not include the mode line or header line or any
        !> horizontal divider.  Port of window.c:Fwindow_body_height (in
        !> canonical lines; PIXELWISE waits for the GUI).
        module function window_body_height(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_body_height

        !> Return the width of WINDOW's text area.
        !> WINDOW must be a live window and defaults to the selected one.  The
        !> return value does not include any vertical dividers, fringes or
        !> marginal areas, or scroll bars.  Port of
        !> window.c:Fwindow_body_width (in canonical columns; PIXELWISE waits
        !> for the GUI).
        module function window_body_width(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_body_width

        !> Whether window W shows a mode line: a live window, not the
        !> minibuffer window, more than one line high.  Port of
        !> window.c:window_wants_mode_line (no `mode_line_format' window
        !> parameter; every buffer has a mode line).
        module function window_wants_mode_line(w) result(wants)
            implicit none(type, external)
            type(window_type), intent(in) :: w
            logical :: wants
        end function window_wants_mode_line

        !> Return the height in pixels of WINDOW's mode-line.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_mode_line_height (a terminal frame: 1 or
        !> 0).
        module function window_mode_line_height(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_mode_line_height

        !> Return the height in pixels of WINDOW's header-line.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_header_line_height (a terminal frame).
        module function window_header_line_height(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_header_line_height

        !> Return the height in pixels of WINDOW's tab-line.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_tab_line_height (a terminal frame).
        module function window_tab_line_height(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_tab_line_height

        !> Return the number of columns by which WINDOW is scrolled from left margin.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_hscroll.
        module function window_hscroll(window, status) result(n)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: n
        end function window_hscroll

        !> Return true when WINDOW is dedicated to its buffer.
        !> More precisely, return the value assigned by the last call of
        !> `set_window_dedication' for WINDOW.  Return DEDICATED_NONE if that
        !> function was never called with WINDOW as its argument, or the value
        !> set by that function was internally reset since its last call.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_dedicated_p.
        module function window_dedication(window, status) result(d)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer :: d
        end function window_dedication

        !> Mark WINDOW as dedicated according to FLAG.
        !> WINDOW must be a live window and defaults to the selected one.  FLAG
        !> DEDICATED_WEAK means mark WINDOW as dedicated to its buffer.  FLAG
        !> DEDICATED_NONE means mark WINDOW as non-dedicated.
        !> As a special case, if FLAG is DEDICATED_STRONG, mark WINDOW as
        !> "strongly" dedicated to its buffer.  Port of
        !> window.c:Fset_window_dedicated_p.
        module subroutine set_window_dedication(window, flag, status)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            integer, intent(in) :: flag
            type(status_type), intent(out) :: status
        end subroutine set_window_dedication

        !> Return the use time of window WINDOW.
        !> WINDOW must specify a live window and defaults to the selected one.
        !> The window with the highest use time is usually the one most recently
        !> selected by calling `select_window' with NORECORD false.  Port of
        !> window.c:Fwindow_use_time.
        module function window_use_time(window, status) result(t)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer(int64) :: t
        end function window_use_time

        !> Mark WINDOW as second most recently used.
        !> WINDOW must specify a live window.
        !> If WINDOW is not selected and the selected window has the highest use
        !> time of all windows, set the use time of WINDOW to that of the selected
        !> window, increase the use time of the selected window by one and return
        !> the new use time of WINDOW.  Otherwise, do nothing and return -1.
        !> Port of window.c:Fwindow_bump_use_time.
        module function window_bump_use_time(window, status) result(t)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            integer(int64) :: t
        end function window_bump_use_time

        !> Return buffers previously shown in WINDOW.
        !> WINDOW must be a live window and defaults to the selected one.
        !> The return value is an array of elements (BUFFER, WINDOW-START,
        !> POS), where BUFFER is a buffer, WINDOW-START is the start position of
        !> the window for that buffer, and POS is a window-specific point value.
        !> Port of window.c:Fwindow_prev_buffers.
        module function window_prev_buffers(window, status) result(list)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(prev_buffer_type), allocatable :: list(:)
        end function window_prev_buffers

        !> Return array of buffers recently re-shown in WINDOW.
        !> WINDOW must be a live window and defaults to the selected one.
        !> Port of window.c:Fwindow_next_buffers.
        module function window_next_buffers(window, status) result(list)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type), allocatable :: list(:)
        end function window_next_buffers

        !> Return live window after WINDOW in the cyclic ordering of windows.
        !> WINDOW must be a live window and defaults to the selected one.  The
        !> optional argument MINIBUF specifies the set of windows to consider.
        !> MINIBUF MINIBUF_ACTIVE or omitted means consider the minibuffer window
        !> only if the minibuffer is active.  MINIBUF_YES means consider the
        !> minibuffer window even if the minibuffer is not active.  MINIBUF_NO
        !> means do not consider the minibuffer window even if the minibuffer is
        !> active.  If you use consistent values for MINIBUF, you can use
        !> `next_window' to iterate through the entire cycle of acceptable
        !> windows, eventually ending up back at the window you started with.
        !> `previous_window' traverses the same cycle, in the reverse order.
        !> Port of window.c:Fnext_window (one frame: ALL-FRAMES makes no
        !> difference and is left out).
        module function next_window(window, minibuf, status) result(r)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            integer, intent(in), optional :: minibuf
            type(status_type), intent(out) :: status
            type(handle_type) :: r
        end function next_window

        !> Return live window before WINDOW in the cyclic ordering of windows.
        !> WINDOW must be a live window and defaults to the selected one.  The
        !> optional argument MINIBUF specifies the set of windows to consider,
        !> as for `next_window'.  Port of window.c:Fprevious_window (one frame).
        module function previous_window(window, minibuf, status) result(r)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            integer, intent(in), optional :: minibuf
            type(status_type), intent(out) :: status
            type(handle_type) :: r
        end function previous_window

        !> Return a list of all live windows.
        !> WINDOW specifies the first window to list and defaults to the selected
        !> window.  Optional argument MINIBUF chooses whether the minibuffer
        !> window counts, as for `next_window'.  If WINDOW is not on the list of
        !> windows returned, some other window will be listed first but no error
        !> occurs.  Port of window.c:Fwindow_list_1 (with window_list_1;
        !> one frame).
        module function window_list_1(window, minibuf, status) result(list)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window
            integer, intent(in), optional :: minibuf
            type(status_type), intent(out) :: status
            type(handle_type), allocatable :: list(:)
        end function window_list_1

        !> Return a list of windows on FRAME, starting with WINDOW.
        !> WINDOW omitted means use the window selected within FRAME.
        !> MINIBUF MINIBUF_YES means include the minibuffer window, even if
        !> it isn't active.  MINIBUF_ACTIVE or omitted means include the
        !> minibuffer window only if it's active.  MINIBUF_NO means never
        !> include the minibuffer window.  Port of window.c:Fwindow_list (one
        !> frame).
        module function window_list(minibuf, window, status) result(list)
            implicit none(type, external)
            integer, intent(in), optional :: minibuf
            type(handle_type), intent(in), optional :: window
            type(status_type), intent(out) :: status
            type(handle_type), allocatable :: list(:)
        end function window_list

        !> Return the part of WINDOW at COORDINATES X and Y.
        !> WINDOW must be a live window and defaults to the selected one.
        !> X and Y are distances measured in characters from the upper-left
        !> corner of the frame.  (0, 0) denotes the character in the upper
        !> left corner of the frame.  If they are in the text portion of
        !> WINDOW, return ON_TEXT and the coordinates relative to the window
        !> in REL_X and REL_Y.  If they are in the mode line of WINDOW, return
        !> ON_MODE_LINE; in the header line, ON_HEADER_LINE; in the tab line,
        !> ON_TAB_LINE; on the border
        !> between WINDOW and its right sibling, ON_VERTICAL_BORDER (Fortmacs's
        !> `vertical-line'); elsewhere, ON_NOTHING.  Port of
        !> window.c:Fcoordinates_in_window_p (a terminal frame).
        module function window_part_at(x, y, window, rel_x, rel_y, status) result(part)
            implicit none(type, external)
            integer, intent(in) :: x, y
            type(handle_type), intent(in), optional :: window
            integer, intent(out) :: rel_x, rel_y
            type(status_type), intent(out) :: status
            integer :: part
        end function window_part_at

        !> Force all windows to be updated on next redisplay.
        !> If optional arg WINDOW is given, force redisplay of that window only.
        !> If BUFFER is given, force redisplay of all windows displaying that
        !> buffer.  Return true if a window was found (always, with neither
        !> argument).  Port of window.c:Fforce_window_update (with
        !> window.c:window_loop for REDISPLAY_BUFFER_WINDOWS): redisplay
        !> recomputes every window, so there is no flag to set.
        module function force_window_update(window, buffer) result(done)
            implicit none(type, external)
            type(handle_type), intent(in), optional :: window, buffer
            logical :: done
        end function force_window_update

        !> Return window containing coordinates X and Y on FRAME.
        !> X and Y are measured in units of canonical columns and rows.
        !> The top left corner of the frame is considered to be column 0, row 0.
        !> Port of window.c:Fwindow_at (one frame).
        module function window_at(x, y) result(window)
            implicit none(type, external)
            integer, intent(in) :: x, y
            type(handle_type) :: window
        end function window_at
    end interface

    ! Side windows, in submodule window_sides_mod; `window_display_buffer'
    ! is in window_display_mod.
    interface
        !> Display BUFFER in WINDOW: TYPE is QR_OTHER when WINDOW existed
        !> already (`reuse'), QR_WINDOW when it is new.  Unless it shows
        !> BUFFER already, or is reused and not a side window, WINDOW is
        !> dedicated as TABLE's `dedicated' entry says, else as
        !> `display_buffer_mark_dedicated' says for a new window; a new
        !> window gets the height and width TABLE's `window_height' and
        !> `window_width' entries give it.  Return WINDOW if BUFFER and
        !> WINDOW are live.  Port of window.el:window--display-buffer.
        module function window_display_buffer(buffer, h, type, status, table) result(window)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer, h
            integer, intent(in) :: type
            type(status_type), intent(out) :: status
            type(display_action_parameters_type), intent(in) :: table
            type(handle_type) :: window
        end function window_display_buffer

        !> The windows of the tree under ROOT in pre-order, depth first: its
        !> live windows, and with INTERNAL its internal windows too.  Port of
        !> window.el:walk-window-tree and walk-window-subtree (the windows
        !> are returned in LIST instead of passed to a function).
        recursive module subroutine walk_window_tree(root, internal, list)
            implicit none(type, external)
            type(handle_type), intent(in) :: root
            logical, intent(in) :: internal
            type(handle_type), allocatable, intent(out) :: list(:)
        end subroutine walk_window_tree

        !> Return the first window on the frame whose `window_side' is
        !> SIDE, or with WINDOW_SIDE_NONE the first side window.  INTERNAL
        !> true means consider internal windows too.  The null handle for none.
        !> Port of window.el:window-with-parameter (for `window_side').
        module function window_with_side(side, internal) result(window)
            implicit none(type, external)
            integer, intent(in) :: side
            logical, intent(in) :: internal
            type(handle_type) :: window
        end function window_with_side

        !> Return the main window of the frame.
        !> If the frame has no side windows, return its root window.
        !> Otherwise, return either an internal non-side window such that
        !> all other non-side windows on the frame descend from it, or the
        !> single live non-side window of the frame.  Port of
        !> window.el:window-main-window.
        module function window_main_window() result(main)
            implicit none(type, external)
            type(handle_type) :: main
        end function window_main_window

        !> Return true if WINDOW should not be used as "other" window.
        !> WINDOW must be a live window.
        !>
        !> Return true if the `no_other_window' parameter of WINDOW is true
        !> and `ignore_window_parameters' is false.  Return false in any
        !> other case.  Port of window.el:window-no-other-p.
        module function is_window_no_other(window) result(p)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            logical :: p
        end function is_window_no_other

        !> Check side windows configuration of the frame.
        !> In a valid side windows configuration there can be at most one
        !> internal side window on each side and all its children must be
        !> live and have the same `window_side' parameter and no other
        !> window with the same `window_side' parameter exists on the
        !> frame.  If there is no such internal window, there may be at
        !> most one window with this side's `window_side' parameter on the
        !> frame.
        !>
        !> If the configuration is invalid, reset the `window_side'
        !> parameters of all windows on the frame.  Port of
        !> window.el:window--sides-check (with window--sides-check-failed,
        !> whose error condition STATUS returns).
        module subroutine check_window_sides(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine check_window_sides

        !> Return true when side windows should appear reversed on the
        !> frame.  This uses some heuristics to guess the user's intentions
        !> when the selected window of the frame is a side window.
        !> Port of window.el:window--sides-reverse-on-frame-p.
        module function is_window_sides_reversed_on_frame() result(p)
            implicit none(type, external)
            logical :: p
        end function is_window_sides_reversed_on_frame

        !> Display BUFFER in a side window of the selected frame.
        !> TABLE is a buffer display action table.  Its `side' and `slot'
        !> entries have a special meaning:
        !>
        !>  `side' denotes the side of the frame where the new window shall
        !>    be located.  Valid values are WINDOW_SIDE_BOTTOM,
        !>    WINDOW_SIDE_RIGHT, WINDOW_SIDE_TOP and WINDOW_SIDE_LEFT.  The
        !>    default is WINDOW_SIDE_BOTTOM.
        !>
        !>  `slot' specifies the window slot where to display BUFFER.  A
        !>    value of zero means use the middle slot on the specified
        !>    side.  A negative value means use a slot preceding (that is,
        !>    above or on the left of) the middle slot.  A positive value
        !>    means use a slot following (that is, below or on the right
        !>    of) the middle slot.  The default is zero.
        !>
        !> If the current frame size or the settings of `window_sides_slots'
        !> do not permit making a new window, a suitable existing window may
        !> be reused and have its `window_slot' parameter value accordingly
        !> modified.
        !>
        !> Unless `display_buffer_mark_dedicated' is not DEDICATED_NONE,
        !> dedicate the side window used to BUFFER (DEDICATED_SIDE) so that
        !> it does not get reused by other `display_buffer' action
        !> functions.  Return the window used for displaying BUFFER in
        !> WINDOW, the null handle if no suitable window can be found.
        !>
        !> This function installs the `window_side' and `window_slot'
        !> parameters.
        !>
        !> This is an action function for buffer display.  It should be
        !> called only by `display_buffer' or a function directly or
        !> indirectly called by the latter.  Port of
        !> window.el:display-buffer-in-side-window.
        module subroutine display_buffer_in_side_window(buffer, table, window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(display_action_parameters_type), intent(in) :: table
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
        end subroutine display_buffer_in_side_window
    end interface

contains

    !> Where the display of `w' starts (1 if it shows no buffer).  Port of
    !> window.c:Fwindow_start.
    integer(int64) function window_start(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status

        pos = 1
        if (w%start%index == 0) return
        call marker_position(w%start, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_start

    !> Make `w' start at `pos'; with `noforce', redisplay may move it to
    !> show point.  Port of window.c:Fset_window_start.
    subroutine set_window_start(w, pos, status, noforce)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noforce

        if (w%start%index == 0) call make_marker(w%start)
        call set_marker(w%start, pos, status, w%buffer)
        w%force_start = .true.
        if (present(noforce)) w%force_start = .not. noforce
        w%window_end_valid = .false.
    end subroutine set_window_start

    !> The window's point marker position.  Port of window.c:Fwindow_point
    !> for a non-selected window (the selected window's point is its
    !> buffer's point).
    integer(int64) function window_point(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status

        pos = 1
        if (w%pointm%index == 0) return
        call marker_position(w%pointm, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_point

    !> Port of window.c:Fset_window_point (for a non-selected window).
    subroutine set_window_point(w, pos, status)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status

        if (w%pointm%index == 0) call make_marker(w%pointm)
        call set_marker(w%pointm, pos, status, w%buffer)
    end subroutine set_window_point

    !> Lines for text: the total minus the mode line, the header line and
    !> the tab line.  Port of window.c:window_body_height (in lines) and
    !> window.c:window_internal_height.
    integer function window_body_lines(w)
        type(window_type), intent(in) :: w

        window_body_lines = w%total_lines
        if (.not. w%mini) window_body_lines = max(0, w%total_lines - 1)
        if (window_wants_header_line(w)) window_body_lines = window_body_lines - 1
        if (window_wants_tab_line(w)) window_body_lines = window_body_lines - 1
    end function window_body_lines

    !> Whether window W shows a header line: its buffer has a
    !> `header_line_format' and the window has room for it besides a text
    !> line and the mode line.  Port of window.c:window_wants_header_line
    !> (no `window_header_line_format' parameter).
    logical function window_wants_header_line(w) result(wants)
        type(window_type), intent(in) :: w
        integer(int64) :: bi
        type(status_type) :: st

        wants = .false.
        if (w%mini .or. w%total_lines <= 2) return
        call resolve_buffer(w%buffer, bi, st)
        if (.not. st%ok()) return
        wants = buffers(bi)%header_line_format%is_set()
    end function window_wants_header_line

    !> Whether window W shows a tab line: its buffer has a
    !> `tab_line_format' and the window has room for it besides a text
    !> line, the mode line and the header line (they prevail).  Port of
    !> window.c:window_wants_tab_line (no `window_tab_line_format'
    !> parameter).
    logical function window_wants_tab_line(w) result(wants)
        type(window_type), intent(in) :: w
        integer(int64) :: bi
        type(status_type) :: st
        integer :: lines

        wants = .false.
        if (w%mini) return
        call resolve_buffer(w%buffer, bi, st)
        if (.not. st%ok()) return
        if (.not. buffers(bi)%tab_line_format%is_set()) return
        ! The mode line, the header line and a text line.
        lines = 2
        if (window_wants_header_line(w)) lines = 3
        wants = w%total_lines > lines
    end function window_wants_tab_line

    !> Free the window's markers.  Not a port (upstream relies on GC).
    subroutine destroy_window(w)
        type(window_type), intent(inout) :: w
        type(status_type) :: status
        integer :: k

        if (w%start%index /= 0) call destroy_marker(w%start, status)
        if (w%pointm%index /= 0) call destroy_marker(w%pointm, status)

        if (allocated(w%prev_buffers)) then
            do k = 1, size(w%prev_buffers)
                call destroy_previous_buffer(w%prev_buffers(k))
            end do

            deallocate (w%prev_buffers)
        end if

        if (allocated(w%next_buffers)) deallocate (w%next_buffers)
        call destroy_quit_restore(w%quit_restore)
        call destroy_quit_restore(w%quit_restore_prev)
        w%start = null_handle()
        w%pointm = null_handle()
        w%buffer = null_handle()
    end subroutine destroy_window

    !> Free the markers of a previous-buffer entry.  Not a port.
    subroutine destroy_previous_buffer(e)
        type(prev_buffer_type), intent(inout) :: e
        type(status_type) :: status

        if (e%start%index /= 0) call destroy_marker(e%start, status)
        if (e%point%index /= 0) call destroy_marker(e%point, status)
        e = prev_buffer_type()
    end subroutine destroy_previous_buffer

    !> Clear a `quit_restore' parameter (set it to none), freeing its
    !> marker.  Not a port.
    subroutine destroy_quit_restore(q)
        type(quit_restore_type), intent(inout) :: q
        type(status_type) :: status

        if (q%quad_point%index /= 0) call destroy_marker(q%quad_point, status)
        q = quit_restore_type()
    end subroutine destroy_quit_restore

    ! ------------------------------------------------------------------
    ! The window registry and tree
    ! ------------------------------------------------------------------

    !> A new window, in no tree and showing no buffer.  Port of
    !> window.c:make_window (with window.c:allocate_window).
    subroutine make_window(h)
        type(handle_type), intent(out) :: h
        type(window_type), allocatable :: grown(:)

        call wtable%acquire(h)

        if (.not. allocated(windows)) then
            allocate (windows(wtable%capacity()))
        else if (size(windows, kind=int64) < wtable%capacity()) then
            allocate (grown(wtable%capacity()))
            grown(1:size(windows)) = windows
            call move_alloc(grown, windows)
        end if

        windows(h%index) = window_type()
    end subroutine make_window

    !> Free window `h': its markers and its slot.  A window that a window
    !> configuration refers to is only marked dead, with its markers
    !> freed, so that the configuration can revive it.  Not a port
    !> (upstream relies on GC).
    subroutine delete_window_object(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status

        if (.not. wtable%is_live(h)) return
        if (windows(h%index)%dead) return

        if (windows(h%index)%config_refs > 0) then
            associate (w => windows(h%index))
                if (w%start%index /= 0) call destroy_marker(w%start, status)
                if (w%pointm%index /= 0) call destroy_marker(w%pointm, status)
                w%start = null_handle()
                w%pointm = null_handle()
                if (w%buffer%index /= 0) w%kept_buffer = w%buffer
                w%buffer = null_handle()
                w%parent = null_handle()
                w%next = null_handle()
                w%prev = null_handle()
                w%child = null_handle()
                w%dead = .true.
            end associate

            return
        end if

        call destroy_window(windows(h%index))
        windows(h%index) = window_type()
        call wtable%release(h, status)
    end subroutine delete_window_object

    !> Window `h', or a null pointer if it doesn't exist.  Valid until the
    !> next make_window.  Not a port.
    function window_ref(h) result(w)
        type(handle_type), intent(in) :: h
        type(window_type), pointer :: w

        w => null()

        if (wtable%is_live(h)) then
            if (.not. windows(h%index)%dead) w => windows(h%index)
        end if
    end function window_ref

    !> Whether `h' is a valid window (live or internal).  Port of
    !> window.c:Fwindow_valid_p.
    logical function is_window_valid(h)
        type(handle_type), intent(in) :: h

        is_window_valid = wtable%is_live(h)
        if (is_window_valid) is_window_valid = .not. windows(h%index)%dead
    end function is_window_valid

    !> Whether `h' is a live window: one that shows a buffer (a leaf).
    !> Port of window.c:Fwindow_live_p.
    logical function is_window_live(h)
        type(handle_type), intent(in) :: h

        is_window_live = wtable%is_live(h)
        if (is_window_live) is_window_live = windows(h%index)%child%index == 0 .and. .not. windows(h%index)%dead
    end function is_window_live

    !> Whether handles A and B name the same window.  Not a port.
    pure logical function same_window(a, b)
        type(handle_type), intent(in) :: a, b

        same_window = a%index == b%index .and. a%generation == b%generation
    end function same_window

    !> The live windows of the tree under `root', in cyclic order (top to
    !> bottom, left to right).  Port of window.c:window_list_1, in part, window.c:window_sub_list and
    !> window.c:get_leaf_windows
    !> (one frame).
    recursive subroutine window_leaves(root, list)
        type(handle_type), intent(in) :: root
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type) :: c

        if (.not. allocated(list)) allocate (list(0))
        if (.not. wtable%is_live(root)) return

        if (windows(root%index)%child%index == 0) then
            list = [list, root]
            return
        end if

        c = windows(root%index)%child

        do while (wtable%is_live(c))
            call window_leaves(c, list)
            c = windows(c%index)%next
        end do
    end subroutine window_leaves

    !> Columns for text in `w': its width less the vertical border.
    !> Not a port.
    integer function window_text_cols(w)
        type(window_type), intent(in) :: w

        window_text_cols = w%total_cols
        if (w%divider) window_text_cols = max(1, w%total_cols - 1)
    end function window_text_cols

    !> Give the tree under `h' the place `top', `left', `lines' by `cols'
    !> on a frame `frame_cols' wide.  Children share the size along their
    !> combination in proportion to their sizes before (equally if they
    !> had none), the last taking what is left.  Not a port: it sizes new
    !> and restored window trees, and the window commands that resize a
    !> combination (upstream's window--resize-this-window, which frame
    !> size changes use through `window_resize_root_window').
    recursive subroutine resize_window_tree(h, top, left, lines, cols, frame_cols)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: top, left, lines, cols, frame_cols
        type(handle_type) :: c
        integer :: n, old_total, k, pos, size_k, given, old_k

        if (.not. wtable%is_live(h)) return

        associate (w => windows(h%index))
            if (w%horizontal) then
                old_total = w%total_cols
            else
                old_total = w%total_lines
            end if

            w%top_line = top
            w%left_col = left
            w%total_lines = lines
            w%total_cols = cols
            w%divider = left + cols < frame_cols
            if (w%child%index == 0) return
        end associate

        ! Count the children and the old sizes along the combination.
        n = 0
        c = windows(h%index)%child

        do while (wtable%is_live(c))
            n = n + 1
            c = windows(c%index)%next
        end do

        pos = 0
        given = 0
        k = 0
        c = windows(h%index)%child

        do while (wtable%is_live(c))
            k = k + 1

            if (windows(h%index)%horizontal) then
                old_k = windows(c%index)%total_cols
                size_k = new_size(cols)
                call resize_window_tree(c, top, left + pos, lines, size_k, frame_cols)
            else
                old_k = windows(c%index)%total_lines
                size_k = new_size(lines)
                call resize_window_tree(c, top + pos, left, size_k, cols, frame_cols)
            end if

            pos = pos + size_k
            c = windows(c%index)%next
        end do

    contains

        !> The new size of the k-th child out of `total'.
        integer function new_size(total)
            integer, intent(in) :: total

            if (k == n) then
                new_size = total - given
            else if (old_total > 0 .and. old_k > 0) then
                new_size = max(1, (old_k*total)/old_total)
            else
                new_size = total/n
            end if

            given = given + new_size
        end function new_size
    end subroutine resize_window_tree

    !> Return the height of WINDOW, or with HORIZONTAL its width, in
    !> lines or columns.  Port of window.el:window-size (total sizes:
    !> windows are sized in whole lines and columns, so ROUND does not
    !> matter).
    integer function window_size(h, horizontal)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal

        if (horizontal) then
            window_size = windows(h%index)%total_cols
        else
            window_size = windows(h%index)%total_lines
        end if
    end function window_size

    !> WINDOW's normal height, or with HORIZONTAL its normal width.
    !> Port of window.c:Fwindow_normal_size.
    real(real64) function window_normal_size(h, horizontal)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal

        if (horizontal) then
            window_normal_size = windows(h%index)%normal_cols
        else
            window_normal_size = windows(h%index)%normal_lines
        end if
    end function window_normal_size

    !> Reset resize values for all windows of the tree under ROOT, a
    !> frame's root window.  This stores the current value of
    !> `window_size' applied with argument HORIZONTAL in the new total
    !> size of all windows of the tree.  Port of
    !> window.el:window--resize-reset and window.el:window--resize-reset-1
    !> (the new normal sizes are local to `window_resize_child_windows').
    recursive subroutine window_resize_reset(root, horizontal)
        type(handle_type), intent(in) :: root
        logical, intent(in) :: horizontal
        type(handle_type) :: sub

        if (.not. wtable%is_live(root)) return
        windows(root%index)%new_total = window_size(root, horizontal)
        sub = windows(root%index)%child

        do while (wtable%is_live(sub))
            call window_resize_reset(sub, horizontal)
            sub = windows(sub%index)%next
        end do
    end subroutine window_resize_reset

    !> Return DELTA if DELTA lines can be added to WINDOW.
    !> HORIZONTAL true means return DELTA if DELTA columns can be added
    !> to WINDOW.  A return value of zero means that no lines (or
    !> columns) can be added to WINDOW.
    !>
    !> This function looks only at WINDOW and, recursively, its child
    !> windows.
    !>
    !> DELTA positive means WINDOW shall be enlarged by DELTA lines or
    !> columns.  If WINDOW cannot be enlarged by DELTA lines or columns
    !> return the maximum value in the range 0..DELTA by which WINDOW
    !> can be enlarged.
    !>
    !> DELTA negative means WINDOW shall be shrunk by -DELTA lines or
    !> columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
    !> return the minimum value in the range DELTA..0 by which WINDOW
    !> can be shrunk.
    !>
    !> IGNORE true means ignore `window_min_height' and
    !> `window_min_width'.  Port of window.el:window-sizable (no
    !> fixed-size windows: `window_size_fixed' is not ported).
    integer function window_sizable(h, delta, horizontal, ignore)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal, ignore
        integer :: min_size, size

        if (delta < 0) then
            min_size = window_min_size(h, horizontal, ignore)
            size = window_size(h, horizontal)

            if (size <= min_size) then
                window_sizable = 0
            else
                window_sizable = max(min_size - size, delta)
            end if
        else
            window_sizable = delta
        end if
    end function window_sizable

    !> Return true if WINDOW can be resized by DELTA lines.
    !> For the meaning of the arguments of this function see
    !> `window_sizable'.  Port of window.el:window-sizable-p.
    logical function is_window_sizable(h, delta, horizontal, ignore)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal, ignore

        if (delta > 0) then
            is_window_sizable = window_sizable(h, delta, horizontal, ignore) >= delta
        else
            is_window_sizable = window_sizable(h, delta, horizontal, ignore) <= delta
        end if
    end function is_window_sizable

    !> Return number of lines by which WINDOW can be shrunk.
    !> Return zero if WINDOW cannot be shrunk.
    !>
    !> HORIZONTAL true means return number of columns by which WINDOW
    !> can be shrunk.  IGNORE true means ignore `window_min_height' and
    !> `window_min_width'.
    !>
    !> NOUP true means don't go up in the window tree, but try to
    !> enlarge windows within WINDOW's combination only.  Port of
    !> window.el:window-min-delta (TRAIL and NODOWN are not ported: the
    !> frame resizing routines don't use them).
    integer function window_min_delta(h, horizontal, ignore, noup)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal, ignore, noup
        integer :: size, minimum

        size = window_size(h, horizontal)
        minimum = window_min_size(h, horizontal, ignore)

        if (size <= minimum) then
            ! WINDOW's size is already at its minimum: there's nothing to
            ! recover.
            window_min_delta = 0
        else
            ! Try to recover whatever WINDOW is larger than its minimum
            ! size.
            window_min_delta = window_min_delta_1(h, size - minimum, horizontal, ignore, noup)
        end if
    end function window_min_delta

    !> Internal function for `window_min_delta'.  Port of
    !> window.el:window--min-delta-1 (no TRAIL, no fixed-size windows).
    recursive integer function window_min_delta_1(h, delta_in, horizontal, ignore, noup) result(delta)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta_in
        logical, intent(in) :: horizontal, ignore, noup
        type(handle_type) :: parent, sub

        delta = delta_in
        parent = windows(h%index)%parent

        ! If we can't go up, return zero.
        if (.not. wtable%is_live(parent)) then
            delta = 0
            return
        end if

        sub = windows(parent%index)%child

        if (windows(parent%index)%horizontal .eqv. horizontal) then
            ! In an iso-combination return DELTA if we find at least one
            ! other child window (none is fixed-size).
            do while (wtable%is_live(sub))
                if (.not. same_window(sub, h)) return
                sub = windows(sub%index)%next
            end do
        else
            ! In an ortho-combination set DELTA to the minimum value by
            ! which other child windows can shrink.
            do while (wtable%is_live(sub))
                if (.not. same_window(sub, h)) then
                    delta = min(delta, max(window_size(sub, horizontal) - window_min_size(sub, horizontal, ignore), 0))
                end if

                sub = windows(sub%index)%next
            end do
        end if

        if (.not. noup) delta = window_min_delta_1(parent, delta, horizontal, ignore, .false.)
    end function window_min_delta_1

    !> Resize WINDOW vertically by DELTA lines.
    !> HORIZONTAL true means resize WINDOW horizontally by DELTA columns.
    !>
    !> IGNORE true means ignore `window_min_height' and
    !> `window_min_width'.  ADD true means add DELTA to the new total
    !> size of WINDOW.
    !>
    !> This function recursively resizes WINDOW's child windows to fit
    !> the new size.  Make sure that WINDOW is `window_sizable' before
    !> calling this function.  Note that this function does not resize
    !> siblings of WINDOW or WINDOW's parent window.  You have to
    !> eventually call `window_resize_apply' in order to make resizing
    !> actually take effect.  Port of window.el:window--resize-this-window
    !> (TRAIL, EDGE and CHAR-SIZE are not ported: the frame resizing
    !> routines don't use them, and windows grow and shrink by whole
    !> lines and columns).
    recursive subroutine window_resize_this_window(h, delta, horizontal, ignore, add)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal, ignore, add
        type(handle_type) :: sub

        ! Add DELTA to the new total size of WINDOW.
        if (add) windows(h%index)%new_total = windows(h%index)%new_total + delta
        sub = windows(h%index)%child
        if (.not. wtable%is_live(sub)) return

        if (windows(h%index)%horizontal .eqv. horizontal) then
            ! In an iso-combination resize child windows according to
            ! their normal sizes.
            call window_resize_child_windows(h, delta, horizontal, ignore)
        else
            ! In an ortho-combination resize each child window by DELTA.
            do while (wtable%is_live(sub))
                call window_resize_this_window(sub, delta, horizontal, ignore, .true.)
                sub = windows(sub%index)%next
            end do
        end if
    end subroutine window_resize_this_window

    !> Resize child windows of window PARENT vertically by DELTA lines.
    !> PARENT must be a vertically combined internal window.
    !>
    !> HORIZONTAL true means resize child windows of PARENT horizontally
    !> by DELTA columns.  In this case PARENT must be a horizontally
    !> combined internal window.  IGNORE true means ignore
    !> `window_min_height' and `window_min_width'.
    !>
    !> The children are resized one line (column) at a time: when
    !> shrinking, the one whose size is largest relative to its normal
    !> size gives a line, as long as it is larger than its minimum size;
    !> when enlarging, the one smallest relative to its normal size gets
    !> it.  Port of window.el:window--resize-child-windows (the
    !> proportional part: WINDOW, TRAIL and EDGE are not ported, and no
    !> window is fixed-size).
    recursive subroutine window_resize_child_windows(parent, delta_in, horizontal, ignore)
        type(handle_type), intent(in) :: parent
        integer, intent(in) :: delta_in
        logical, intent(in) :: horizontal, ignore
        ! The state of each child while resizing (upstream's new_normal):
        ! none, the lines it can still give with its factor, its factor,
        ! or no line left to give (`skip').
        integer, parameter :: NEW_NORMAL_NONE = 0, NEW_NORMAL_SHRINK = 1, NEW_NORMAL_GROW = 2, NEW_NORMAL_SKIP = 3
        type(handle_type), allocatable :: subs(:)
        integer, allocatable :: state(:), min_delta(:)
        real(real64), allocatable :: factor(:)
        type(handle_type) :: sub
        integer :: n, i, delta, parent_total, best_window, best_delta, sub_delta
        logical :: has_best

        delta = delta_in
        parent_total = window_size(parent, horizontal) + delta
        ! The children, first to last.
        n = 0
        sub = windows(parent%index)%child

        do while (wtable%is_live(sub))
            n = n + 1
            sub = windows(sub%index)%next
        end do

        allocate (subs(n), state(n), min_delta(n), factor(n))
        state = NEW_NORMAL_NONE
        min_delta = 0
        factor = 0.0_real64
        sub = windows(parent%index)%child

        do i = 1, n
            subs(i) = sub
            sub = windows(sub%index)%next
        end do

        ! Resize all windows proportionally.
        do i = n, 1, -1
            if (delta < 0) then
                ! When shrinking store the number of lines/cols we can get
                ! from this window here together with the total/normal size
                ! factor.
                state(i) = NEW_NORMAL_SHRINK
                min_delta(i) = window_min_delta(subs(i), horizontal, ignore, .true.)
                factor(i) = real(window_size(subs(i), horizontal), real64)/parent_total &
                            - window_normal_size(subs(i), horizontal)
            else if (delta > 0) then
                ! When enlarging store the total/normal size factor only.
                state(i) = NEW_NORMAL_GROW
                factor(i) = real(window_size(subs(i), horizontal), real64)/parent_total &
                            - window_normal_size(subs(i), horizontal)
            end if
        end do

        has_best = .false.
        best_window = 0

        if (delta < 0) then
            ! Shrink windows by delta.
            has_best = .true.

            do while (has_best .and. delta /= 0)
                has_best = .false.

                do i = n, 1, -1
                    if (state(i) == NEW_NORMAL_SHRINK .and. min_delta(i) > 0) then
                        if (.not. has_best) then
                            has_best = .true.
                            best_window = i
                        else if (factor(i) > factor(best_window)) then
                            best_window = i
                        end if
                    end if
                end do

                if (has_best) then
                    i = best_window
                    best_delta = min(min_delta(i), 1, -delta)
                    delta = delta + best_delta
                    windows(subs(i)%index)%new_total = windows(subs(i)%index)%new_total - best_delta

                    if (min_delta(i) == best_delta) then
                        ! We can't shrink best-window any further.
                        state(i) = NEW_NORMAL_SKIP
                    else
                        min_delta(i) = min_delta(i) - best_delta
                        factor(i) = real(windows(subs(i)%index)%new_total, real64)/parent_total &
                                    - window_normal_size(subs(i), horizontal)
                    end if
                end if
            end do
        else if (delta > 0) then
            ! Enlarge windows by delta.
            has_best = .true.

            do while (has_best .and. delta /= 0)
                has_best = .false.

                do i = n, 1, -1
                    if (state(i) == NEW_NORMAL_GROW) then
                        if (.not. has_best) then
                            has_best = .true.
                            best_window = i
                        else if (factor(i) < factor(best_window)) then
                            best_window = i
                        end if
                    end if
                end do

                if (has_best) then
                    i = best_window
                    best_delta = min(delta, 1)
                    delta = delta - best_delta
                    windows(subs(i)%index)%new_total = windows(subs(i)%index)%new_total + best_delta
                    factor(i) = real(windows(subs(i)%index)%new_total, real64)/parent_total &
                                - window_normal_size(subs(i), horizontal)
                end if
            end do
        end if

        if (.not. has_best) return

        do i = n, 1, -1
            ! Resize this window's child windows (back-engineering delta
            ! from sub's old and new total sizes).
            sub_delta = windows(subs(i)%index)%new_total - window_size(subs(i), horizontal)
            if (sub_delta /= 0) call window_resize_this_window(subs(i), sub_delta, horizontal, ignore, .false.)
        end do
    end subroutine window_resize_child_windows

    !> Resize root window WINDOW vertically by DELTA lines.
    !> HORIZONTAL true means resize root window WINDOW horizontally by
    !> DELTA columns.
    !>
    !> IGNORE true means ignore any restrictions imposed by
    !> `window_min_height' or `window_min_width' settings.
    !>
    !> This function is only called by the frame resizing routines.  It
    !> resizes windows proportionally and never deletes any windows.
    !> Port of window.el:window--resize-root-window (which
    !> window.c:resize_root_window calls; DELTA in lines or columns,
    !> no fixed-size windows).
    subroutine window_resize_root_window(h, delta, horizontal, ignore)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal, ignore

        if (.not. wtable%is_live(h)) return

        if (is_window_sizable(h, delta, horizontal, ignore)) then
            call window_resize_reset(h, horizontal)
            call window_resize_this_window(h, delta, horizontal, ignore, .true.)
        end if
    end subroutine window_resize_root_window

    !> Return true if setting WINDOW's total height (width if HORIZONTAL)
    !> to its new total size would result in correct heights (widths) for
    !> window W and recursively all child windows of W.
    !>
    !> Note: This function does not check any of `window_min_height' or
    !> `window_min_width'.  It does check that window sizes do not drop
    !> below one line (two columns).  Port of window.c:window_resize_check
    !> (in lines and columns).
    recursive logical function window_resize_check(h, horizontal) result(ok)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        type(handle_type) :: c
        integer :: remaining

        ok = .false.
        c = windows(h%index)%child

        if (.not. wtable%is_live(c)) then
            ! A leaf window.  Make sure it's not too small.
            if (horizontal) then
                ok = windows(h%index)%new_total >= window_safe_min_width
            else
                ok = windows(h%index)%new_total >= window_safe_min_height
            end if
        else if (windows(h%index)%horizontal .eqv. horizontal) then
            ! The sum of the sizes of the child windows of W must equal
            ! W's size.
            remaining = windows(h%index)%new_total

            do while (wtable%is_live(c))
                if (.not. window_resize_check(c, horizontal)) return
                remaining = remaining - windows(c%index)%new_total
                if (remaining < 0) return
                c = windows(c%index)%next
            end do

            ok = remaining == 0
        else
            ! All child windows of W must have the same size as W.
            do while (wtable%is_live(c))
                if (windows(c%index)%new_total /= windows(h%index)%new_total) return
                if (.not. window_resize_check(c, horizontal)) return
                c = windows(c%index)%next
            end do

            ok = .true.
        end if
    end function window_resize_check

    !> Set WINDOW's total height (total width if HORIZONTAL) to its new
    !> total size for window W and recursively all child windows of W.
    !> Also calculate and assign the new vertical (horizontal) start
    !> positions of each of these windows, and whether each ends before
    !> the right edge of a frame FRAME_COLS wide (`divider').
    !>
    !> This function does not perform any error checks.  Make sure you
    !> have run `window_resize_check' on W before applying this function.
    !> Port of window.c:window_resize_apply (in lines and columns; the
    !> frame resizing routines leave no new normal size to assign, and
    !> FRAME_WINDOW_CHANGE is not ported).
    recursive subroutine window_resize_apply(h, horizontal, frame_cols)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        integer, intent(in) :: frame_cols
        type(handle_type) :: c
        integer :: edge

        associate (w => windows(h%index))
            if (horizontal) then
                w%total_cols = w%new_total
                w%divider = w%left_col + w%total_cols < frame_cols
                edge = w%left_col
            else
                w%total_lines = w%new_total
                edge = w%top_line
            end if

            c = w%child
        end associate

        if (.not. wtable%is_live(c)) then
            windows(h%index)%window_end_valid = .false.
            return
        end if

        do while (wtable%is_live(c))
            if (horizontal) then
                windows(c%index)%left_col = edge
            else
                windows(c%index)%top_line = edge
            end if

            call window_resize_apply(c, horizontal, frame_cols)

            if (windows(h%index)%horizontal .eqv. horizontal) then
                if (horizontal) then
                    edge = edge + windows(c%index)%total_cols
                else
                    edge = edge + windows(c%index)%total_lines
                end if
            end if

            c = windows(c%index)%next
        end do
    end subroutine window_resize_apply

    !> Free window `h' and the windows under it.  Not a port (GC upstream).
    recursive subroutine delete_window_tree(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: c, nxt

        if (.not. wtable%is_live(h)) return
        c = windows(h%index)%child

        do while (wtable%is_live(c))
            nxt = windows(c%index)%next
            call delete_window_tree(c)
            c = nxt
        end do

        call delete_window_object(h)
    end subroutine delete_window_tree

    !> Put window `new' in `old''s place in the tree (same parent and
    !> siblings), leaving `old' out of it.  Port of window.c:replace_window.
    subroutine replace_window(old, new)
        type(handle_type), intent(in) :: old, new

        associate (o => windows(old%index), n => windows(new%index))
            n%parent = o%parent
            n%next = o%next
            n%prev = o%prev
            n%top_line = o%top_line
            n%left_col = o%left_col
            n%total_lines = o%total_lines
            n%total_cols = o%total_cols
            n%divider = o%divider
            n%normal_cols = o%normal_cols
            o%normal_cols = 1.0_real64
            n%normal_lines = o%normal_lines
            o%normal_lines = 1.0_real64
            if (wtable%is_live(o%next)) windows(o%next%index)%prev = new

            if (wtable%is_live(o%prev)) then
                windows(o%prev%index)%next = new
            else if (wtable%is_live(o%parent)) then
                windows(o%parent%index)%child = new
            end if

            o%parent = null_handle()
            o%next = null_handle()
            o%prev = null_handle()
        end associate
    end subroutine replace_window

    !> Split window `old' in two: make `new' (a new window showing the
    !> buffer of `refer', by default `old', which must then be live) below
    !> it or, with `horizontal', to its right; with `before', above it or
    !> to its left.  `new_size' is the size of the new
    !> window in lines or columns.  If `old' isn't already in a combination
    !> of that kind, an internal window replaces it there, with `old' and
    !> `new' as its children.  `root' is the frame's root window, updated
    !> if it was `old'.  `normal_size' is the new window's normal size;
    !> `old' keeps the rest of its own (all of the new internal window's).
    !> Without it the new window gets half: 0.5 of a new internal window,
    !> else half of `old''s normal size.  With `resize' the space comes
    !> from all the windows of `old''s combination instead, whose new
    !> sizes (`new_total') the caller has set; it then sets the normal
    !> sizes too.  With `new_parent' an internal window is made even if `old'
    !> is in a combination of that kind (`window_combination_limit' t).
    !> Port of
    !> window.c:Fsplit_window_internal (the sizes are worked out by the
    !> caller, window.el:split-window, which also gives `old' its new
    !> normal size and the default NORMAL-SIZE when SIZE is omitted).
    !> With `before' the new window goes above `old' (to its left with
    !> `horizontal'): window.c:Fsplit_window_internal's SIDE `above' or
    !> `left'.
    subroutine split_window_internal(old, horizontal, new_size, root, new, status, refer, normal_size, before, resize, new_parent)
        type(handle_type), intent(in) :: old
        logical, intent(in) :: horizontal
        integer, intent(in) :: new_size
        type(handle_type), intent(inout) :: root
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: refer
        real(real64), intent(in), optional :: normal_size
        logical, intent(in), optional :: before, resize, new_parent
        type(handle_type) :: parent, nxt, prv, r, c
        integer :: old_size, taken
        logical :: combine, above, resizing
        real(real64) :: new_normal, old_normal

        status = status_type()
        r = old
        if (present(refer)) r = refer
        above = .false.
        if (present(before)) above = before
        resizing = .false.
        if (present(resize)) resizing = resize
        parent = windows(old%index)%parent
        combine = .false.
        if (wtable%is_live(parent)) combine = windows(parent%index)%horizontal .eqv. horizontal

        if (present(new_parent)) then
            if (new_parent) combine = .false.
        end if

        ! The normal sizes of the new window and of old (window.el:split-window).
        old_normal = 1.0_real64
        if (combine) old_normal = window_normal_size(old, horizontal)

        if (present(normal_size)) then
            new_normal = normal_size
        else if (combine) then
            new_normal = old_normal/2.0_real64
        else
            new_normal = 0.5_real64
        end if

        if (.not. combine) then
            ! A new internal window takes old's place, with old as its child.
            resizing = .false.
            call make_window(parent)
            call replace_window(old, parent)
            windows(parent%index)%horizontal = horizontal
            windows(parent%index)%child = old
            windows(old%index)%parent = parent
            if (same_window(root, old)) root = parent
        end if

        call make_window(new)

        associate (o => windows(old%index), n => windows(new%index))
            n%parent = parent

            if (above) then
                prv = o%prev
                n%prev = prv

                if (wtable%is_live(prv)) then
                    windows(prv%index)%next = new
                else
                    windows(parent%index)%child = new
                end if

                n%next = old
                o%prev = new
            else
                nxt = o%next
                n%next = nxt
                n%prev = old
                o%next = new
                if (wtable%is_live(nxt)) windows(nxt%index)%prev = new
            end if

            n%top_line = o%top_line
            n%left_col = o%left_col
            n%total_lines = o%total_lines
            n%total_cols = o%total_cols
            n%divider = o%divider
        end associate

        if (resizing) then
            ! The other windows of the combination have their new sizes:
            ! the new window gets the rest, and window_resize_apply places
            ! them all.
            taken = 0
            c = windows(parent%index)%child

            do while (wtable%is_live(c))
                if (.not. same_window(c, new)) taken = taken + windows(c%index)%new_total
                c = windows(c%index)%next
            end do

            if (horizontal) then
                windows(new%index)%new_total = windows(parent%index)%total_cols - taken
            else
                windows(new%index)%new_total = windows(parent%index)%total_lines - taken
            end if

            associate (rw => windows(root%index))
                call window_resize_apply(parent, horizontal, rw%left_col + rw%total_cols)
            end associate
        else
            associate (o => windows(old%index), n => windows(new%index))
                if (horizontal) then
                    old_size = o%total_cols - new_size
                    o%total_cols = old_size
                    n%total_cols = new_size

                    if (above) then
                        o%left_col = n%left_col + new_size
                        n%divider = .true.
                    else
                        n%left_col = o%left_col + old_size
                        o%divider = .true.
                    end if

                    o%normal_cols = old_normal - new_normal
                    n%normal_cols = new_normal
                else
                    old_size = o%total_lines - new_size
                    o%total_lines = old_size
                    n%total_lines = new_size

                    if (above) then
                        o%top_line = n%top_line + new_size
                    else
                        n%top_line = o%top_line + old_size
                    end if

                    o%normal_lines = old_normal - new_normal
                    n%normal_lines = new_normal
                end if
            end associate
        end if

        call set_window_buffer_internal(new, windows(r%index)%buffer, .true.)
        ! (set_window_buffer with run_hooks_p.)
        call execute_window_scroll_functions(new, status)
    end subroutine split_window_internal

    !> Take window `h' out of the tree: its space goes to the sibling
    !> before it (or else after it), and a parent left with one child is
    !> replaced by that child.  `root' is the frame's root window, updated
    !> as needed.  `heir' is the sibling that got the space; `h' itself is
    !> not freed.  Port of window.c:Fdelete_window_internal, in part (the
    !> tree part).
    subroutine remove_window_from_tree(h, root, heir, frame_cols)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(inout) :: root
        type(handle_type), intent(out) :: heir
        integer, intent(in) :: frame_cols
        type(handle_type) :: parent, only
        integer :: top, left, lines, cols

        heir = null_handle()
        parent = windows(h%index)%parent
        if (.not. wtable%is_live(parent)) return

        associate (w => windows(h%index))
            if (wtable%is_live(w%prev)) then
                heir = w%prev
            else
                heir = w%next
            end if

            ! Unlink.
            if (wtable%is_live(w%prev)) then
                windows(w%prev%index)%next = w%next
            else
                windows(parent%index)%child = w%next
            end if

            if (wtable%is_live(w%next)) windows(w%next%index)%prev = w%prev

            ! The heir grows over the space, and takes its normal size
            ! (window.el:delete-window).
            associate (e => windows(heir%index))
                top = min(e%top_line, w%top_line)
                left = min(e%left_col, w%left_col)

                if (windows(parent%index)%horizontal) then
                    lines = e%total_lines
                    cols = e%total_cols + w%total_cols
                    e%normal_cols = e%normal_cols + w%normal_cols
                else
                    lines = e%total_lines + w%total_lines
                    cols = e%total_cols
                    e%normal_lines = e%normal_lines + w%normal_lines
                end if
            end associate

            w%parent = null_handle()
            w%next = null_handle()
            w%prev = null_handle()
        end associate

        call resize_window_tree(heir, top, left, lines, cols, frame_cols)
        ! A combination of one is no combination.
        only = windows(parent%index)%child

        if (.not. wtable%is_live(windows(only%index)%next)) then
            call replace_window(parent, only)
            if (same_window(root, parent)) root = only
            windows(parent%index)%child = null_handle()
            call delete_window_object(parent)
        end if
    end subroutine remove_window_from_tree
end module window_mod
