! tab_line_mod.f90 -- window-local tabs with window buffers.
! Ported from GNU Emacs 31.1 lisp/tab-line.el (in part: see
! docs/status/tab_line_mod.md).
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> To enable this mode, run `M-x global_tab_line_mode'.  `tab_line_mode'
!> gives the windows showing a buffer a tab line, the top line of the
!> window (the buffer's `tab_line_format', drawn by redisplay above the
!> header line): a row of tabs, one for each buffer the window showed
!> (`tab_line_tabs_function'), scrolled to show the selected one.  The
!> tabs are switched with C-x <left> and C-x <right> and closed with
!> `close_tab_line_tab'.  What is left out is listed in
!> docs/status/tab_line_mod.md.
module tab_line_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_keymap_ref, command_type, vtag, xsymbol, symbols, V_NAME
    use data_mod, only: choice_setting_type, logical_setting_type, forward_setting
    use execute_interactively_mod, only: current_prefix_arg, prefix_numeric_value, prefix_arg_text, find_command, &
                                         PREFIX_NUMBER, PREFIX_NONE
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use derived_mod, only: major_mode_type, major_mode_named, is_derived_mode
    use hooks_mod, only: hook_type
    use basic_subprograms_mod, only: after_change_major_mode_hook
    use character_mod, only: string_char_and_length
    use faces_mod, only: named_face_type, display_when, face_attrs, face_attributes_type, face_box, &
                         FACE_BOX_RELEASED_BUTTON, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use keymap_mod, only: make_sparse_keymap, is_keymap_live, destroy_keymap, keymap_set, set_repeat_map, key_parse, &
                          define_key, lookup_key
    use buffer_mod, only: buffers, current, current_buffer, resolve_buffer, is_buffer_live, set_buffer_internal, &
                          buffer_list, header_line_segment_type, header_line_format_type, close_buffer, is_buffer_modified, &
                          force_mode_line_update
    use utility_functions_mod, only: count_characters, is_string_less_text
    use multilingual_environment_utilities_mod, only: truncate_string_to_width
    use coding_mod, only: is_char_encodable, terminal_coding_system
    use search_mod, only: string_match
    use minibuffer_reading_mod, only: is_minibuffer
    use frame_mod, only: selected_frame, selected_window
    use window_mod, only: windows, window_text_cols, switch_to_buffer, bury_buffer, previous_buffer, next_buffer, &
                          switch_to_prev_buffer, switch_to_next_buffer, select_window, destroy_previous_buffer, same_handle, &
                          same_window, is_window_live, prev_buffer_type, tab_line_hscroll_type, BK_NONE
    use display_engine_mod, only: message, glyph_columns, is_mode_line_window_selected
    use project_mod, only: project_type, get_current_project
    implicit none(type, external)
    private

    public :: tab_line_mode, global_tab_line_mode, initialize_tab_line
    public :: format_tab_line, make_tab_line_format_template, force_tab_line_update
    public :: get_tab_line_tabs_window_buffers, get_tab_line_tabs_fixed_window_buffers, get_tab_line_tabs_mode_buffers, &
              get_tab_line_tabs_buffer_groups, get_tab_line_tabs_buffer_list
    public :: get_tab_line_tab_name_buffer, get_tab_line_tab_name_truncated_buffer, format_tab_line_tab_name_default
    public :: get_tab_line_tab_face_special, get_tab_line_tab_face_modified, get_tab_line_tab_face_inactive_alternating, &
              get_tab_line_tab_face_group
    public :: get_tab_line_tabs_buffer_group_by_mode, get_tab_line_tabs_buffer_group_by_project, &
              get_tab_line_tabs_buffer_group_name, is_tab_line_buffer_name_less
    public :: keep_all_tab_line_buffers, remove_excluded_tab_line_buffers, is_tab_line_tab_modified
    public :: switch_to_tab_line_previous_tab, switch_to_tab_line_next_tab, move_tab_line_tab_forward, &
              move_tab_line_tab_backward, close_tab_line_tab, close_tab_line_other_tabs, scroll_tab_line, &
              select_tab_line_tab_buffer

    ! ------------------------------------------------------------------
    ! Tabs
    ! ------------------------------------------------------------------

    !> A tab of the tab line: a buffer (IS_BUFFER, upstream a buffer), or
    !> a tab with a NAME (upstream an association list): with BUFFER the
    !> null handle or its buffer, SELECTED whether it is the selected tab,
    !> GROUP_TAB whether it is the group tab shown before the buffers of a
    !> group, GROUP (allocated) the group that selecting it shows (with the
    !> groups for a group tab, else the group's buffers), and
    !> CLOSES_BUFFER whether closing it closes its buffer.  A tab with no
    !> buffer and no name stands for none.  Not a port.
    type, public :: tab_line_tab_type
        logical :: is_buffer = .false.
        type(handle_type) :: buffer
        character(len=:), allocatable :: name
        logical :: selected = .false.
        logical :: group_tab = .false.
        character(len=:), allocatable :: group
        logical :: closes_buffer = .false.
    end type tab_line_tab_type

    abstract interface
        !> The tabs to show in the tab line of WINDOW (upstream the
        !> selected window), whose buffer is current
        !> (`tab_line_tabs_function').  Not a port.
        recursive subroutine tab_line_tabs_procedure(window, tabs)
            import :: handle_type, tab_line_tab_type
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        end subroutine tab_line_tabs_procedure

        !> The name of the tab of BUFFER among TABS
        !> (`tab_line_tab_name_function').  Not a port.
        recursive subroutine tab_line_tab_name_procedure(buffer, tabs, name)
            import :: handle_type, tab_line_tab_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(tab_line_tab_type), intent(in) :: tabs(:)
            character(len=:), allocatable, intent(out) :: name
        end subroutine tab_line_tab_name_procedure

        !> The text of tab TABS(K) in the tab line of WINDOW
        !> (`tab_line_tab_name_format_function'): SEGMENTS, and SELECTED
        !> whether it is the selected tab (upstream its `selected' text
        !> property).  Not a port.
        recursive subroutine tab_line_tab_name_format_procedure(window, tabs, k, segments, selected)
            import :: handle_type, tab_line_tab_type, header_line_segment_type
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(tab_line_tab_type), intent(in) :: tabs(:)
            integer, intent(in) :: k
            type(header_line_segment_type), allocatable, intent(out) :: segments(:)
            logical, intent(out) :: selected
        end subroutine tab_line_tab_name_format_procedure

        !> Change FACES, the faces of tab TABS(K) (the first above the
        !> others), SELECTED whether it is the selected tab (an element of
        !> `tab_line_tab_face_functions').  Not a port.
        recursive subroutine tab_line_tab_face_procedure(tabs, k, selected, faces)
            import :: tab_line_tab_type
            implicit none(type, external)
            type(tab_line_tab_type), intent(in) :: tabs(:)
            integer, intent(in) :: k
            logical, intent(in) :: selected
            character(len=40), allocatable, intent(inout) :: faces(:)
        end subroutine tab_line_tab_face_procedure

        !> The buffers of WINDOW's tab line from all buffers
        !> (`tab_line_tabs_buffer_list_function').  Not a port.
        recursive subroutine tab_line_buffers_procedure(window, list)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(handle_type), allocatable, intent(out) :: list(:)
        end subroutine tab_line_buffers_procedure

        !> Keep in LIST the buffers to show in the tab line
        !> (`tab_line_tabs_window_buffers_filter_function').  Not a port.
        recursive subroutine tab_line_buffer_filter_procedure(list)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), allocatable, intent(inout) :: list(:)
        end subroutine tab_line_buffer_filter_procedure

        !> The group of BUFFER, unallocated for none
        !> (`tab_line_tabs_buffer_group_function').  Not a port.
        recursive subroutine tab_line_buffer_group_procedure(buffer, group)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: group
        end subroutine tab_line_buffer_group_procedure

        !> Whether buffer A comes before buffer B
        !> (`tab_line_tabs_buffer_group_sort_function').  Not a port.
        recursive logical function tab_line_buffer_order_procedure(a, b)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: a, b
        end function tab_line_buffer_order_procedure

        !> Whether group name A comes before group name B
        !> (`tab_line_tabs_buffer_groups_sort_function').  Not a port.
        pure logical function tab_line_group_order_procedure(a, b)
            implicit none(type, external)
            character(len=*), intent(in) :: a, b
        end function tab_line_group_order_procedure

        !> Close TAB (`tab_line_close_tab_function%procedure'): a tab with
        !> no buffer and no name when the current buffer has none.
        !> Not a port.
        recursive subroutine tab_line_close_tab_procedure(tab, status)
            import :: tab_line_tab_type, status_type
            implicit none(type, external)
            type(tab_line_tab_type), intent(in) :: tab
            type(status_type), intent(out) :: status
        end subroutine tab_line_close_tab_procedure
    end interface

    public :: tab_line_tabs_procedure, tab_line_tab_name_procedure, tab_line_tab_name_format_procedure, &
              tab_line_tab_face_procedure, tab_line_buffers_procedure, tab_line_buffer_filter_procedure, &
              tab_line_buffer_group_procedure, tab_line_buffer_order_procedure, tab_line_group_order_procedure, &
              tab_line_close_tab_procedure

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Tab line face for selected tab.  Port of tab-line.el tab-line-tab.
    type(named_face_type), target, public :: tab_line_tab
    !> Tab line face for non-selected tab.  Port of tab-line.el
    !> tab-line-tab-inactive.
    type(named_face_type), target, public :: tab_line_tab_inactive
    !> Alternate face for inactive tab-line tabs.
    !> Applied to alternating tabs when option
    !> `tab_line_tab_face_functions' includes procedure
    !> `get_tab_line_tab_face_inactive_alternating'.  Port of tab-line.el
    !> tab-line-tab-inactive-alternate.
    type(named_face_type), target, public :: tab_line_tab_inactive_alternate
    !> Face for special (i.e. non-file-backed) tabs.
    !> Applied when option `tab_line_tab_face_functions' includes
    !> procedure `get_tab_line_tab_face_special'.  Port of tab-line.el
    !> tab-line-tab-special.
    type(named_face_type), target, public :: tab_line_tab_special
    !> Face for modified tabs.
    !> Applied when option `tab_line_tab_face_functions' includes
    !> procedure `get_tab_line_tab_face_modified'.  Port of tab-line.el
    !> tab-line-tab-modified.
    type(named_face_type), target, public :: tab_line_tab_modified
    !> Face for group tabs.
    !> Applied when option `tab_line_tab_face_functions' includes
    !> procedure `get_tab_line_tab_face_group'.  Port of tab-line.el
    !> tab-line-tab-group.
    type(named_face_type), target, public :: tab_line_tab_group
    !> Tab line face for tab with current buffer in selected window.
    !> Port of tab-line.el tab-line-tab-current.
    type(named_face_type), target, public :: tab_line_tab_current
    !> Tab line face for highlighting.  Port of tab-line.el
    !> tab-line-highlight.
    type(named_face_type), target, public :: tab_line_highlight
    !> Tab line face for highlighting of the close button.  Port of
    !> tab-line.el tab-line-close-highlight.
    type(named_face_type), target, public :: tab_line_close_highlight

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> An element of a list of tab face procedures.  Not a port.
    type :: tab_line_tab_face_entry_type
        procedure(tab_line_tab_face_procedure), pointer, nopass :: face_function => null()
    end type tab_line_tab_face_entry_type

    !> A list of tab face procedures (`tab_line_tab_face_functions'),
    !> called in order.  Not a port.
    type, public :: tab_line_tab_face_functions_type
        type(tab_line_tab_face_entry_type), allocatable :: entries(:)
    contains
        !> Add a procedure at the end.
        procedure :: append => tab_face_functions_append
        !> Add a procedure at the start.
        procedure :: prepend => tab_face_functions_prepend
        !> Remove a procedure.
        procedure :: remove => tab_face_functions_remove
        !> Whether a procedure is in the list.
        procedure :: has => tab_face_functions_has
    end type tab_line_tab_face_functions_type

    !> Functions called to modify tab faces.
    !> Each procedure is called with the tabs, the index of the tab, whether
    !> the tab is selected, and the faces returned by the previously called
    !> modifier, which it changes.  The default is
    !> `get_tab_line_tab_face_modified' and `get_tab_line_tab_face_special';
    !> `get_tab_line_tab_face_inactive_alternating' and
    !> `get_tab_line_tab_face_group' are the others
    !> (`call tab_line_tab_face_functions%append(get_tab_line_tab_face_group)').
    !> Port of tab-line.el tab-line-tab-face-functions.
    type(tab_line_tab_face_functions_type), public :: tab_line_tab_face_functions

    !> If true, show the "New tab" button in the tab line.  Port of
    !> tab-line.el tab-line-new-button-show (its :set function waits for
    !> Customize).
    logical, public :: tab_line_new_button_show = .true.

    !> Button for creating a new tab.  Port of tab-line.el
    !> tab-line-new-button.
    character(len=:), allocatable, public :: tab_line_new_button

    !> An element of a list of tabs procedures.  Not a port.
    type :: tab_line_tabs_entry_type
        procedure(tab_line_tabs_procedure), pointer, nopass :: tabs_function => null()
    end type tab_line_tabs_entry_type

    !> A list of tabs procedures (`tab_line_new_button_functions').
    !> Not a port.
    type, public :: tab_line_tabs_functions_type
        type(tab_line_tabs_entry_type), allocatable :: entries(:)
    contains
        !> Add a procedure at the end.
        procedure :: append => tabs_functions_append
        !> Remove a procedure.
        procedure :: remove => tabs_functions_remove
        !> Whether a procedure is in the list.
        procedure :: has => tabs_functions_has
    end type tab_line_tabs_functions_type

    !> Functions of `tab_line_tabs_function' for which to show a new button.
    !> Port of tab-line.el tab-line-new-button-functions.
    type(tab_line_tabs_functions_type), public :: tab_line_new_button_functions

    !> Values of `tab_line_close_button_show' besides .true. and .false.
    integer, parameter, public :: TAB_LINE_CLOSE_BUTTON_SHOW_SELECTED = 2, TAB_LINE_CLOSE_BUTTON_SHOW_NON_SELECTED = 3

    !> Defines where to show the close tab button.
    !> If .true., show the close tab button on all tabs.
    !> If TAB_LINE_CLOSE_BUTTON_SHOW_SELECTED, show it only on the selected tab.
    !> If TAB_LINE_CLOSE_BUTTON_SHOW_NON_SELECTED, show it only on non-selected tab.
    !> If .false., don't show it at all.  Port of tab-line.el
    !> tab-line-close-button-show (its :set function waits for Customize).
    type(choice_setting_type), public :: tab_line_close_button_show = &
                                         choice_setting_type(name="tab_line_close_button_show", value=.true.)

    !> Button for closing the clicked tab.  Port of tab-line.el
    !> tab-line-close-button.
    character(len=:), allocatable, public :: tab_line_close_button

    !> If true, the close button appearance will change when its buffer is modified.
    !> Port of tab-line.el tab-line-close-modified-button-show (its :set
    !> function waits for Customize).
    logical, public :: tab_line_close_modified_button_show = .true.

    !> Button for closing the clicked tab when tab is modified.  Port of
    !> tab-line.el tab-line-close-modified-button.
    character(len=:), allocatable, public :: tab_line_close_modified_button

    !> Button for scrolling horizontally to the left.  Port of tab-line.el
    !> tab-line-left-button.
    character(len=:), allocatable, public :: tab_line_left_button

    !> Button for scrolling horizontally to the right.  Port of tab-line.el
    !> tab-line-right-button.
    character(len=:), allocatable, public :: tab_line_right_button

    !> String that delimits tabs (unallocated for the default: a space on
    !> a window system, "|" on a terminal).  Port of tab-line.el
    !> tab-line-separator.
    character(len=:), allocatable, public :: tab_line_separator

    !> Function to get a tab name.
    !> The procedure is called with the buffer whose tab's name is
    !> requested, and the list of all tabs (empty when not given).  The
    !> result of this procedure is cached.  The choices are
    !> `get_tab_line_tab_name_buffer' (the default, the buffer name) and
    !> `get_tab_line_tab_name_truncated_buffer' (the truncated buffer
    !> name).  Port of tab-line.el tab-line-tab-name-function (its :set
    !> function waits for Customize).
    procedure(tab_line_tab_name_procedure), pointer, public :: tab_line_tab_name_function => null()

    !> Maximum length of the tab name from the current buffer.
    !> Effective when `tab_line_tab_name_function' is customized
    !> to `get_tab_line_tab_name_truncated_buffer'.  Port of tab-line.el
    !> tab-line-tab-name-truncated-max.
    integer, public :: tab_line_tab_name_truncated_max = 20

    !> Function to get a list of tabs to display in the tab line.
    !> This procedure gives the tabs to show: buffers whose names will be
    !> displayed, or named tabs.  By default, use procedure
    !> `get_tab_line_tabs_fixed_window_buffers' that gives the buffers
    !> associated with the selected window where buffers always keep the
    !> original order after switching buffers.
    !> When `get_tab_line_tabs_mode_buffers', give the buffers
    !> with the same major mode as the current buffer.
    !> When `get_tab_line_tabs_buffer_groups', give the buffers
    !> grouped by `tab_line_tabs_buffer_group_function'.
    !> `get_tab_line_tabs_window_buffers' gives the window buffers.
    !> The result of this procedure is cached.  Port of tab-line.el
    !> tab-line-tabs-function (its :set function waits for Customize).
    procedure(tab_line_tabs_procedure), pointer, public :: tab_line_tabs_function => null()

    !> Function to return a global list of buffers.
    !> Used only for `get_tab_line_tabs_mode_buffers' and
    !> `get_tab_line_tabs_buffer_groups'.  Port of tab-line.el
    !> tab-line-tabs-buffer-list-function.
    procedure(tab_line_buffers_procedure), pointer, public :: tab_line_tabs_buffer_list_function => null()

    !> Function to add a buffer to the appropriate group of tabs.
    !> Takes a buffer as argument and should give a group name.
    !> If it gives none, the buffer has no group, so "No group"
    !> is displayed instead of a group name and the buffer is not grouped
    !> together with other buffers.
    !> If the value is `get_tab_line_tabs_buffer_group_by_mode',
    !> use mode-to-group mappings in `tab_line_tabs_buffer_groups'
    !> to group by major mode.  If the value is
    !> `get_tab_line_tabs_buffer_group_by_project' use the project name
    !> as a group name.  Port of tab-line.el
    !> tab-line-tabs-buffer-group-function (its :set function waits for
    !> Customize).
    procedure(tab_line_buffer_group_procedure), pointer, public :: tab_line_tabs_buffer_group_function => null()

    !> Function to sort buffers in a group (null: don't sort; the default,
    !> `is_tab_line_buffer_name_less', sorts by name alphabetically).
    !> Port of tab-line.el tab-line-tabs-buffer-group-sort-function (its
    !> :set function waits for Customize).
    procedure(tab_line_buffer_order_procedure), pointer, public :: tab_line_tabs_buffer_group_sort_function => null()

    !> Function to sort group names (null: don't sort; the default,
    !> `is_string_less_text', sorts alphabetically).  Port of tab-line.el
    !> tab-line-tabs-buffer-groups-sort-function (its :set function waits
    !> for Customize).
    procedure(tab_line_group_order_procedure), pointer, public :: tab_line_tabs_buffer_groups_sort_function => null()

    !> A group of major modes for the tab line: the modes whose mode name
    !> matches REGULAR_EXPRESSION belong to the group NAME.  Not a port.
    type, public :: tab_line_buffer_group_type
        character(len=120) :: regular_expression = ""
        character(len=40) :: name = ""
    end type tab_line_buffer_group_type

    !> How to group various major modes together in the tab line.
    !> Each element has a regular expression and a group name
    !> (`tab_line_buffer_group_type(regular_expression="Text", name="Text")').
    !> If the major mode's name matches the regular expression, it belongs
    !> to the group.  The default is mouse.el's mouse-buffer-menu-mode-groups;
    !> a major mode in none of them has a separate group named the same as
    !> the mode.  Port of tab-line.el tab-line-tabs-buffer-groups.
    type(tab_line_buffer_group_type), allocatable, public :: tab_line_tabs_buffer_groups(:)

    !> Filter which buffers should be displayed in the tab line.
    !> `keep_all_tab_line_buffers' (the default) shows all buffers,
    !> `remove_excluded_tab_line_buffers' omits excluded buffers.  Port of
    !> tab-line.el tab-line-tabs-window-buffers-filter-function.
    procedure(tab_line_buffer_filter_procedure), pointer, public :: tab_line_tabs_window_buffers_filter_function => null()

    !> Function to format a tab name.
    !> The procedure is called with the window, the list of all the tabs and
    !> the index of the tab whose name to format; it gives the formatted
    !> tab name to display in the tab line.  The result of this procedure
    !> is cached.  Port of tab-line.el tab-line-tab-name-format-function
    !> (its :set function waits for Customize).
    procedure(tab_line_tab_name_format_procedure), pointer, public :: tab_line_tab_name_format_function => null()

    !> Allow or disallow automatic horizontal scrolling of the tab line.
    !> True means the tab lines are automatically scrolled horizontally to make
    !> the selected tab visible.  Port of tab-line.el tab-line-auto-hscroll.
    logical, public :: tab_line_auto_hscroll = .true.

    !> Wrap tabs on tab switch while cycling.
    !> If true, `switch_to_tab_line_previous_tab' in the first tab
    !> switches to the last tab and `switch_to_tab_line_next_tab' in the
    !> last tab switches to the first tab.  This variable is not consulted
    !> when `tab_line_tabs_function' is `get_tab_line_tabs_window_buffers'.
    !> Port of tab-line.el tab-line-switch-cycling.
    logical, public :: tab_line_switch_cycling = .true.

    !> The setting `tab_line_close_tab_function': a named constant or a
    !> procedure (R6-37: `%procedure').  Not a port.
    type, extends(choice_setting_type), public :: tab_line_close_tab_function_choice_setting_type
        procedure(tab_line_close_tab_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => close_tab_function_clear_kinds
        procedure :: other_value => close_tab_function_other_value
        procedure :: kind_hint => close_tab_function_kind_hint
    end type tab_line_close_tab_function_choice_setting_type

    !> Values of `tab_line_close_tab_function'.
    integer, parameter, public :: TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER = 2, TAB_LINE_CLOSE_TAB_FUNCTION_CLOSE_BUFFER = 3

    !> What to do upon closing a tab on the tab line.
    !> If TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER, put the tab's buffer at
    !> the end of the list of all buffers, which effectively hides the
    !> buffer's tab from the tab line.
    !> If TAB_LINE_CLOSE_TAB_FUNCTION_CLOSE_BUFFER, closes the tab's buffer.
    !> If its procedure is set (`tab_line_close_tab_function%procedure =>
    !> f'), it is called with the tab as its argument.
    !> This option is useful when `tab_line_tabs_function' is
    !> `get_tab_line_tabs_window_buffers' or
    !> `get_tab_line_tabs_fixed_window_buffers'.  Port of tab-line.el
    !> tab-line-close-tab-function.
    type(tab_line_close_tab_function_choice_setting_type), public :: tab_line_close_tab_function = &
                                               tab_line_close_tab_function_choice_setting_type(name="tab_line_close_tab_function", &
                                                                                   choice=TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER, &
                                                                                            true_valid=.false., false_valid=.false.)

    !> Define specific tab-line key bindings.
    !> If .true., the default, key mappings for switching and moving tabs
    !> are defined.  If .false., do not define any key mappings.  Port of
    !> tab-line.el tab-line-define-keys (read when `global_tab_line_mode'
    !> is turned on or off; its :set function waits for Customize).
    logical, public :: tab_line_define_keys = .true.

    ! ------------------------------------------------------------------
    ! Modes and keymaps
    ! ------------------------------------------------------------------

    !> An element of `tab_line_exclude_modes'.  Not a port.
    type :: tab_line_mode_entry_type
        type(major_mode_type), pointer :: mode => null()
    end type tab_line_mode_entry_type

    !> A list of major modes (`tab_line_exclude_modes').  Not a port.
    type, public :: tab_line_exclude_modes_type
        type(tab_line_mode_entry_type), allocatable :: entries(:)
    contains
        !> Add a major mode at the end.
        procedure :: append => exclude_modes_append
        !> Remove a major mode.
        procedure :: remove => exclude_modes_remove
    end type tab_line_exclude_modes_type

    !> List of major modes for which the tab-line display is not enabled.
    !> Buffers under any of these major modes will not show the tab line in
    !> their windows, even if `global_tab_line_mode' is enabled
    !> (`call tab_line_exclude_modes%append(major_modes%dired_mode)').
    !> Port of tab-line.el tab-line-exclude-modes.
    type(tab_line_exclude_modes_type), public :: tab_line_exclude_modes

    !> True means the current buffer gets no tab line from
    !> `global_tab_line_mode' (it is buffer-local whenever it is set).
    !> Port of tab-line.el tab-line-exclude.
    type(logical_setting_type), target, public :: tab_line_exclude = logical_setting_type(name="tab_line_exclude")

    !> Hook run after entering or leaving `tab_line_mode'.  Port of
    !> tab-line.el tab-line-mode-hook.
    type(hook_type), target, public :: tab_line_mode_hook = hook_type(name="tab_line_mode_hook")

    !> Hook run after entering or leaving `global_tab_line_mode'.  Port of
    !> tab-line.el global-tab-line-mode-hook.
    type(hook_type), target, public :: global_tab_line_mode_hook = hook_type(name="global_tab_line_mode_hook")

    !> Keymap for keys of `tab_line_mode'.  Port of tab-line.el
    !> tab-line-mode-map.
    type(handle_type), public :: tab_line_mode_map

    !> Keymap to repeat tab/buffer cycling.  Used in `repeat_mode'.
    !> Port of tab-line.el tab-line-switch-repeat-map.
    type(handle_type), public :: tab_line_switch_repeat_map

    ! The global C-x map, whose keys `tab_line_define_keys' changes.
    type(handle_type) :: ctl_x_map

    ! Whether `tab_line_mode' was set explicitly in the current buffer
    ! (define-globalized-minor-mode's tab-line-mode-set-explicitly, a
    ! buffer-local variable that clear_all_local_variables clears), and
    ! whether `global_tab_line_mode' is turning it on (its
    ! -suppress-set-explicitly).
    type(logical_setting_type), target :: tab_line_mode_set_explicitly = &
                                          logical_setting_type(name="tab_line_mode_set_explicitly")
    logical :: suppress_set_explicitly = .false.

    ! ------------------------------------------------------------------
    ! The cache
    ! ------------------------------------------------------------------

    ! The tab line made last for a window (its `tab_line_cache'
    ! parameter): its cache keys (tab-line-cache-key-default: the tabs, the
    ! name of the window's buffer, the window's `tab_line_hscroll', whether
    ! the window was the selected one, and whether its buffer was a
    ! modified file-visiting one) and what it showed.
    type :: tab_line_cache_type
        type(handle_type) :: window
        type(tab_line_tab_type), allocatable :: tabs(:)
        character(len=:), allocatable :: buffer_name
        type(tab_line_hscroll_type) :: hscroll
        logical :: selected = .false., modified = .false.
        type(header_line_segment_type), allocatable :: segments(:)
    end type tab_line_cache_type

    type(tab_line_cache_type), allocatable :: tab_line_caches(:)

    ! One string of the tab line: a separator and a tab's text (whose
    ! segments the separator starts), the index of its tab and whether
    ! the tab is the selected one.
    type :: tab_line_string_type
        type(header_line_segment_type), allocatable :: segments(:)
        integer :: tab = 0
        logical :: selected = .false.
    end type tab_line_string_type

contains

    ! ------------------------------------------------------------------
    ! Lists in settings
    ! ------------------------------------------------------------------

    !> Add FACE_FUNCTION at the end of SELF.  Not a port.
    subroutine tab_face_functions_append(self, face_function)
        class(tab_line_tab_face_functions_type), intent(inout) :: self
        procedure(tab_line_tab_face_procedure) :: face_function
        type(tab_line_tab_face_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (self%has(face_function)) return
        entry%face_function => face_function
        self%entries = [self%entries, entry]
    end subroutine tab_face_functions_append

    !> Add FACE_FUNCTION at the start of SELF.  Not a port.
    subroutine tab_face_functions_prepend(self, face_function)
        class(tab_line_tab_face_functions_type), intent(inout) :: self
        procedure(tab_line_tab_face_procedure) :: face_function
        type(tab_line_tab_face_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (self%has(face_function)) return
        entry%face_function => face_function
        self%entries = [entry, self%entries]
    end subroutine tab_face_functions_prepend

    !> Remove FACE_FUNCTION from SELF.  Not a port.
    subroutine tab_face_functions_remove(self, face_function)
        class(tab_line_tab_face_functions_type), intent(inout) :: self
        procedure(tab_line_tab_face_procedure) :: face_function
        type(tab_line_tab_face_entry_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(self%entries)) return
        allocate (kept(0))

        do k = 1, size(self%entries)
            if (.not. associated(self%entries(k)%face_function, face_function)) kept = [kept, self%entries(k)]
        end do

        call move_alloc(kept, self%entries)
    end subroutine tab_face_functions_remove

    !> Whether FACE_FUNCTION is in SELF.  Not a port.
    logical function tab_face_functions_has(self, face_function) result(has)
        class(tab_line_tab_face_functions_type), intent(in) :: self
        procedure(tab_line_tab_face_procedure) :: face_function
        integer :: k

        has = .false.
        if (.not. allocated(self%entries)) return

        do k = 1, size(self%entries)
            if (associated(self%entries(k)%face_function, face_function)) then
                has = .true.
                return
            end if
        end do
    end function tab_face_functions_has

    !> Add TABS_FUNCTION at the end of SELF.  Not a port.
    subroutine tabs_functions_append(self, tabs_function)
        class(tab_line_tabs_functions_type), intent(inout) :: self
        procedure(tab_line_tabs_procedure) :: tabs_function
        type(tab_line_tabs_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (self%has(tabs_function)) return
        entry%tabs_function => tabs_function
        self%entries = [self%entries, entry]
    end subroutine tabs_functions_append

    !> Remove TABS_FUNCTION from SELF.  Not a port.
    subroutine tabs_functions_remove(self, tabs_function)
        class(tab_line_tabs_functions_type), intent(inout) :: self
        procedure(tab_line_tabs_procedure) :: tabs_function
        type(tab_line_tabs_entry_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(self%entries)) return
        allocate (kept(0))

        do k = 1, size(self%entries)
            if (.not. associated(self%entries(k)%tabs_function, tabs_function)) kept = [kept, self%entries(k)]
        end do

        call move_alloc(kept, self%entries)
    end subroutine tabs_functions_remove

    !> Whether TABS_FUNCTION is in SELF.  Not a port.
    logical function tabs_functions_has(self, tabs_function) result(has)
        class(tab_line_tabs_functions_type), intent(in) :: self
        procedure(tab_line_tabs_procedure) :: tabs_function
        integer :: k

        has = .false.
        if (.not. allocated(self%entries)) return

        do k = 1, size(self%entries)
            if (associated(self%entries(k)%tabs_function, tabs_function)) then
                has = .true.
                return
            end if
        end do
    end function tabs_functions_has

    !> Add major mode MODE at the end of SELF.  Not a port.
    subroutine exclude_modes_append(self, mode)
        class(tab_line_exclude_modes_type), intent(inout) :: self
        type(major_mode_type), intent(in), target :: mode
        type(tab_line_mode_entry_type) :: entry
        integer :: k

        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do k = 1, size(self%entries)
            if (associated(self%entries(k)%mode, mode)) return
        end do

        entry%mode => mode
        self%entries = [self%entries, entry]
    end subroutine exclude_modes_append

    !> Remove major mode MODE from SELF.  Not a port.
    subroutine exclude_modes_remove(self, mode)
        class(tab_line_exclude_modes_type), intent(inout) :: self
        type(major_mode_type), intent(in), target :: mode
        type(tab_line_mode_entry_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(self%entries)) return
        allocate (kept(0))

        do k = 1, size(self%entries)
            if (.not. associated(self%entries(k)%mode, mode)) kept = [kept, self%entries(k)]
        end do

        call move_alloc(kept, self%entries)
    end subroutine exclude_modes_remove

    !> Whether the major mode named NAME is in `tab_line_exclude_modes'.
    !> Not a port.
    logical function is_excluded_mode(name) result(excluded)
        character(len=*), intent(in) :: name
        integer :: k

        excluded = .false.
        if (.not. allocated(tab_line_exclude_modes%entries)) return

        do k = 1, size(tab_line_exclude_modes%entries)
            if (trim(tab_line_exclude_modes%entries(k)%mode%name) == name) then
                excluded = .true.
                return
            end if
        end do
    end function is_excluded_mode

    !> Forget the procedure of `tab_line_close_tab_function'.  Not a port.
    subroutine close_tab_function_clear_kinds(self)
        class(tab_line_close_tab_function_choice_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine close_tab_function_clear_kinds

    !> The procedure of `tab_line_close_tab_function' as help shows it; ""
    !> when none is set.  Not a port.
    function close_tab_function_other_value(self) result(text)
        class(tab_line_close_tab_function_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function close_tab_function_other_value

    !> The assignment that sets the procedure `tab_line_close_tab_function'
    !> takes, for the error of a rejected value.  Not a port.
    function close_tab_function_kind_hint(self, number) result(text)
        class(tab_line_close_tab_function_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function close_tab_function_kind_hint

    ! ------------------------------------------------------------------
    ! Tab names
    ! ------------------------------------------------------------------

    !> The name of buffer slot of BUFFER, "" for a dead buffer.  Not a port.
    function name_of(buffer) result(name)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: name
        integer(int64) :: bi
        type(status_type) :: status

        name = ""
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return
        if (allocated(buffers(bi)%name)) name = buffers(bi)%name
    end function name_of

    !> The major mode of buffer slot BI (`major_mode', fundamental_mode
    !> while none is set).  Not a port.
    function major_mode_of(bi) result(name)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: name

        name = "fundamental_mode"
        if (allocated(buffers(bi)%major_mode)) name = buffers(bi)%major_mode
    end function major_mode_of

    !> Generate tab name from BUFFER.
    !> Reduce tab width proportionally to space taken by other tabs.
    !> This procedure can be overridden by changing the value of the
    !> variable `tab_line_tab_name_function'.  Port of
    !> tab-line.el:tab-line-tab-name-buffer.
    recursive subroutine get_tab_line_tab_name_buffer(buffer, tabs, name)
        type(handle_type), intent(in) :: buffer
        type(tab_line_tab_type), intent(in) :: tabs(:)
        character(len=:), allocatable, intent(out) :: name

        name = name_of(buffer)
        if (size(tabs) < 0) name = ""
    end subroutine get_tab_line_tab_name_buffer

    !> Generate tab name from BUFFER, truncating it as needed.
    !> Truncate it to the length specified by `tab_line_tab_name_truncated_max'.
    !> If truncated, append the default ellipsis.  Port of
    !> tab-line.el:tab-line-tab-name-truncated-buffer (with
    !> `tab-line-tab-name-ellipsis' t, the default ellipsis).
    recursive subroutine get_tab_line_tab_name_truncated_buffer(buffer, tabs, name)
        type(handle_type), intent(in) :: buffer
        type(tab_line_tab_type), intent(in) :: tabs(:)
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable :: tab_name

        tab_name = name_of(buffer)

        if (count_characters(tab_name) < tab_line_tab_name_truncated_max) then
            name = tab_name
        else
            name = truncate_string_to_width(tab_name, tab_line_tab_name_truncated_max, default_ellipsis=.true.)
        end if

        if (size(tabs) < 0) name = ""
    end subroutine get_tab_line_tab_name_truncated_buffer

    ! ------------------------------------------------------------------
    ! The tabs
    ! ------------------------------------------------------------------

    !> TAB, a tab of BUFFER (upstream the buffer itself).  Not a port.
    function buffer_tab(buffer) result(tab)
        type(handle_type), intent(in) :: buffer
        type(tab_line_tab_type) :: tab

        tab%is_buffer = .true.
        tab%buffer = buffer
    end function buffer_tab

    !> Append BUFFER to LIST unless it is in it.  Not a port.
    subroutine append_unique(list, buffer)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: buffer

        if (has_buffer(list, buffer)) return
        list = [list, buffer]
    end subroutine append_unique

    !> Whether BUFFER is in LIST.  Not a port.
    logical function has_buffer(list, buffer) result(has)
        type(handle_type), intent(in) :: list(:)
        type(handle_type), intent(in) :: buffer
        integer :: k

        has = .false.

        do k = 1, size(list)
            if (same_handle(list(k), buffer)) then
                has = .true.
                return
            end if
        end do
    end function has_buffer

    !> The index of BUFFER in LIST, 0 if it is not in it.  Not a port.
    integer function buffer_position(list, buffer) result(k)
        type(handle_type), intent(in) :: list(:)
        type(handle_type), intent(in) :: buffer

        do k = 1, size(list)
            if (same_handle(list(k), buffer)) return
        end do

        k = 0
    end function buffer_position

    !> The buffers of all tabs: TABS that are buffers or have a buffer.
    !> Not a port.
    function tab_buffers(tabs) result(list)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        type(handle_type), allocatable :: list(:)
        integer :: k

        allocate (list(0))

        do k = 1, size(tabs)
            if (tabs(k)%buffer%index /= 0) list = [list, tabs(k)%buffer]
        end do
    end function tab_buffers

    !> The buffers the tab lines of WINDOW can show: the current buffer, the
    !> buffers WINDOW showed before and the other buffers, without those
    !> that are dead or whose names start with a space.  Port of
    !> tab-line.el:tab-line-tabs-buffer-list (`window-prev-buffers' of
    !> WINDOW).
    recursive subroutine get_tab_line_tabs_buffer_list(window, list)
        type(handle_type), intent(in) :: window
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: all(:), order(:)
        character(len=:), allocatable :: name
        integer :: k

        allocate (all(0))
        call append_unique(all, current_buffer())

        if (allocated(windows(window%index)%prev_buffers)) then
            do k = 1, size(windows(window%index)%prev_buffers)
                call append_unique(all, windows(window%index)%prev_buffers(k)%buffer)
            end do
        end if

        order = buffer_list()

        do k = 1, size(order)
            call append_unique(all, order(k))
        end do

        allocate (list(0))

        do k = 1, size(all)
            if (.not. is_buffer_live(all(k))) cycle
            name = name_of(all(k))

            if (len(name) > 0) then
                if (name(1:1) == " ") cycle
            end if

            list = [list, all(k)]
        end do
    end subroutine get_tab_line_tabs_buffer_list

    !> The buffers of WINDOW's tab line from all buffers, by
    !> `tab_line_tabs_buffer_list_function'.  Not a port.
    recursive subroutine global_buffers(window, list)
        type(handle_type), intent(in) :: window
        type(handle_type), allocatable, intent(out) :: list(:)
        procedure(tab_line_buffers_procedure), pointer :: buffers_function

        buffers_function => tab_line_tabs_buffer_list_function

        if (associated(buffers_function)) then
            call buffers_function(window, list)
        else
            call get_tab_line_tabs_buffer_list(window, list)
        end if

        if (.not. allocated(list)) allocate (list(0))
    end subroutine global_buffers

    !> Return the buffers with the same major mode as the current buffer.
    !> Port of tab-line.el:tab-line-tabs-mode-buffers.
    recursive subroutine get_tab_line_tabs_mode_buffers(window, tabs)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        type(handle_type), allocatable :: all(:), kept(:)
        type(major_mode_type), pointer :: mode
        character(len=:), allocatable :: mode_name
        type(status_type) :: status
        integer(int64) :: saved, bi
        integer :: k
        logical :: derived

        mode_name = major_mode_of(current)
        mode => major_mode_named(mode_name)
        call global_buffers(window, all)
        allocate (kept(0))
        saved = current

        do k = 1, size(all)
            call resolve_buffer(all(k), bi, status)
            if (.not. status%ok()) cycle
            call set_buffer_internal(bi)

            if (associated(mode)) then
                derived = is_derived_mode(mode)
            else
                derived = major_mode_of(bi) == mode_name
            end if

            if (derived) kept = [kept, all(k)]
        end do

        if (saved > 0 .and. saved /= current) call set_buffer_internal(saved)
        call sort_buffers_by_name(kept)
        allocate (tabs(size(kept)))

        do k = 1, size(kept)
            tabs(k) = buffer_tab(kept(k))
        end do
    end subroutine get_tab_line_tabs_mode_buffers

    !> Sort LIST by buffer name (string<), keeping the order of equal
    !> names.  Not a port.
    subroutine sort_buffers_by_name(list)
        type(handle_type), intent(inout) :: list(:)
        type(handle_type) :: item
        integer :: i, j

        do i = 2, size(list)
            item = list(i)
            j = i - 1

            do while (j >= 1)
                if (.not. is_string_less_text(name_of(item), name_of(list(j)))) exit
                list(j + 1) = list(j)
                j = j - 1
            end do

            list(j + 1) = item
        end do
    end subroutine sort_buffers_by_name

    !> Return true if TAB's buffer is modified.
    !> TAB is a buffer, or a tab with a buffer.  Port of
    !> tab-line.el:tab-line-tab-modified-p.
    logical function is_tab_line_tab_modified(tab) result(modified)
        type(tab_line_tab_type), intent(in) :: tab
        type(status_type) :: status
        integer(int64) :: bi

        modified = .false.
        if (tab%buffer%index == 0) return
        call resolve_buffer(tab%buffer, bi, status)
        if (.not. status%ok()) return
        if (.not. allocated(buffers(bi)%filename)) return
        call is_buffer_modified(tab%buffer, modified, status)
        if (.not. status%ok()) modified = .false.
    end function is_tab_line_tab_modified

    !> Group tab buffers by major mode.  Port of
    !> tab-line.el:tab-line-tabs-buffer-group-by-mode.
    recursive subroutine get_tab_line_tabs_buffer_group_by_mode(buffer, group)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: group
        character(len=:), allocatable :: mode
        type(status_type) :: status
        integer(int64) :: bi
        integer :: k

        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) bi = current
        ! (format-mode-line mode-name)
        mode = "Fundamental"
        if (allocated(buffers(bi)%mode_name)) mode = buffers(bi)%mode_name

        if (allocated(tab_line_tabs_buffer_groups)) then
            do k = 1, size(tab_line_tabs_buffer_groups)
                if (string_match(trim(tab_line_tabs_buffer_groups(k)%regular_expression), mode, status, &
                                 inhibit_modify=.true.) > 0) then
                    group = trim(tab_line_tabs_buffer_groups(k)%name)
                    return
                end if
            end do
        end if

        group = mode
    end subroutine get_tab_line_tabs_buffer_group_by_mode

    !> Group tab buffers by project name.  Port of
    !> tab-line.el:tab-line-tabs-buffer-group-by-project.
    recursive subroutine get_tab_line_tabs_buffer_group_by_project(buffer, group)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: group
        class(project_type), allocatable :: project
        type(status_type) :: status
        integer(int64) :: saved, bi

        group = "No project"
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return
        saved = current
        call set_buffer_internal(bi)
        call get_current_project(project, status)
        if (allocated(project) .and. status%ok()) group = project%name()
        if (saved > 0 .and. saved /= current) call set_buffer_internal(saved)
    end subroutine get_tab_line_tabs_buffer_group_by_project

    !> The group of BUFFER: `tab_line_tabs_buffer_group_function', or by
    !> mode when it is null; unallocated for none.  Port of
    !> tab-line.el:tab-line-tabs-buffer-group-name.
    recursive subroutine get_tab_line_tabs_buffer_group_name(buffer, group)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: group
        procedure(tab_line_buffer_group_procedure), pointer :: group_function

        group_function => tab_line_tabs_buffer_group_function

        if (associated(group_function)) then
            call group_function(buffer, group)
        else
            call get_tab_line_tabs_buffer_group_by_mode(buffer, group)
        end if
    end subroutine get_tab_line_tabs_buffer_group_name

    !> Whether buffer A's name comes before buffer B's (string<).  Port of
    !> tab-line.el:tab-line-tabs-buffer-group-sort-by-name.
    recursive logical function is_tab_line_buffer_name_less(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = is_string_less_text(name_of(a), name_of(b))
    end function is_tab_line_buffer_name_less

    !> Whether the groups A and B are the same (both none, or the same name).
    !> Not a port.
    logical function is_same_group(a, b) result(same)
        character(len=:), allocatable, intent(in) :: a, b

        same = allocated(a) .eqv. allocated(b)

        if (same .and. allocated(a)) then
            if (allocated(b)) same = a == b .and. len(a) == len(b)
        end if
    end function is_same_group

    !> Return a list of tabs that should be displayed in the tab line.
    !> By default return a list of buffers grouped by major mode,
    !> according to `tab_line_tabs_buffer_groups'.
    !> If set, `tab_line_tabs_buffer_group_function' is used to
    !> generate the group name.  Port of
    !> tab-line.el:tab-line-tabs-buffer-groups (the groups and the group
    !> tab select through their GROUP, the buffers close their buffer).
    recursive subroutine get_tab_line_tabs_buffer_groups(window, tabs)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        type(handle_type), allocatable :: all(:), kept(:)
        type(tab_line_tab_type), allocatable :: none(:)
        character(len=:), allocatable :: group, name, parameter, group_name
        type(tab_line_tab_type) :: tab
        integer :: k

        call global_buffers(window, all)

        if (windows(window%index)%tab_line%groups) then
            call group_tabs()
            return
        end if

        if (allocated(windows(window%index)%tab_line%group)) parameter = windows(window%index)%tab_line%group
        call get_tab_line_tabs_buffer_group_name(current_buffer(), group_name)

        if (allocated(parameter)) then
            group = parameter
        else if (allocated(group_name)) then
            group = group_name
        else
            group = "No group"
        end if

        if (is_same_group(parameter, group_name)) then
            if (allocated(windows(window%index)%tab_line%group)) deallocate (windows(window%index)%tab_line%group)
        end if

        allocate (tabs(1))
        tabs(1)%name = group
        tabs(1)%group_tab = .true.
        tabs(1)%group = group
        allocate (kept(0))

        do k = 1, size(all)
            call get_tab_line_tabs_buffer_group_name(all(k), name)

            if (allocated(name)) then
                if (name == group .and. len(name) == len(group)) kept = [kept, all(k)]
            end if
        end do

        if (associated(tab_line_tabs_buffer_group_sort_function)) call sort_buffers(kept)
        allocate (none(0))

        do k = 1, size(kept)
            tab = tab_line_tab_type()
            call tab_line_tab_name_function(kept(k), none, tab%name)
            tab%selected = same_handle(kept(k), current_buffer())
            tab%buffer = kept(k)
            tab%closes_buffer = .true.
            tabs = [tabs, tab]
        end do

    contains

        !> The tabs of the groups (`tab_line_groups'): one per group of
        !> the buffers, in the order seq-group-by gives (by the last buffer
        !> of each), sorted by `tab_line_tabs_buffer_groups_sort_function'.
        subroutine group_tabs()
            type(tab_line_tab_type) :: group_tab
            character(len=:), allocatable :: selected_group
            character(len=200), allocatable :: groups(:)
            character(len=200) :: swap
            integer :: i, j, n

            allocate (groups(0))

            do i = size(all), 1, -1
                call get_tab_line_tabs_buffer_group_name(all(i), name)
                if (.not. allocated(name)) cycle
                if (any(groups == name)) cycle
                groups = [character(len=200) :: name, groups]
            end do

            n = size(groups)

            if (associated(tab_line_tabs_buffer_groups_sort_function)) then
                do i = 2, n
                    swap = groups(i)
                    j = i - 1

                    do while (j >= 1)
                        if (.not. tab_line_tabs_buffer_groups_sort_function(trim(swap), trim(groups(j)))) exit
                        groups(j + 1) = groups(j)
                        j = j - 1
                    end do

                    groups(j + 1) = swap
                end do
            end if

            if (allocated(windows(window%index)%tab_line%group)) selected_group = windows(window%index)%tab_line%group
            allocate (tabs(0))

            do i = 1, n
                group_tab = tab_line_tab_type()
                group_tab%name = trim(groups(i))

                if (allocated(selected_group)) then
                    group_tab%selected = selected_group == trim(groups(i)) .and. len(selected_group) == len_trim(groups(i))
                end if

                group_tab%group = trim(groups(i))
                tabs = [tabs, group_tab]
            end do
        end subroutine group_tabs

        !> Sort LIST by `tab_line_tabs_buffer_group_sort_function', keeping
        !> the order of the others.
        subroutine sort_buffers(list)
            type(handle_type), intent(inout) :: list(:)
            type(handle_type) :: item
            integer :: i, j

            do i = 2, size(list)
                item = list(i)
                j = i - 1

                do while (j >= 1)
                    if (.not. tab_line_tabs_buffer_group_sort_function(item, list(j))) exit
                    list(j + 1) = list(j)
                    j = j - 1
                end do

                list(j + 1) = item
            end do
        end subroutine sort_buffers
    end subroutine get_tab_line_tabs_buffer_groups

    !> Show all buffers: keep LIST as it is.  Port of the default of
    !> tab-line.el tab-line-tabs-window-buffers-filter-function (identity).
    recursive subroutine keep_all_tab_line_buffers(list)
        type(handle_type), allocatable, intent(inout) :: list(:)

        if (.not. allocated(list)) allocate (list(0))
    end subroutine keep_all_tab_line_buffers

    !> Filter LIST to remove excluded buffers from the list.
    !> Intended to be used in `tab_line_tabs_window_buffers_filter_function'.
    !> Port of tab-line.el:tab-line-tabs-non-excluded
    !> (`tab-line-exclude-buffers' is Deferred).
    recursive subroutine remove_excluded_tab_line_buffers(list)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), allocatable :: kept(:)
        type(major_mode_type), pointer :: mode
        type(status_type) :: status
        integer(int64) :: saved, bi
        integer :: k
        logical :: excluded

        if (.not. allocated(list)) allocate (list(0))
        allocate (kept(0))
        saved = current

        do k = 1, size(list)
            call resolve_buffer(list(k), bi, status)

            if (.not. status%ok()) then
                kept = [kept, list(k)]
                cycle
            end if

            excluded = is_excluded_mode(major_mode_of(bi))
            mode => major_mode_named(major_mode_of(bi))

            if (associated(mode)) then
                if (mode%tab_line_exclude) excluded = .true.
            end if

            call set_buffer_internal(bi)
            if (tab_line_exclude%value) excluded = .true.
            if (.not. excluded) kept = [kept, list(k)]
        end do

        if (saved > 0 .and. saved /= current) call set_buffer_internal(saved)
        call move_alloc(kept, list)
    end subroutine remove_excluded_tab_line_buffers

    !> The window buffers of WINDOW: those it showed before, the buffer
    !> it shows and those it showed after (`switch_to_prev_buffer'), live
    !> and once each.  Not a port.
    subroutine window_buffers(window, list)
        type(handle_type), intent(in) :: window
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: next(:), prev(:)
        type(handle_type) :: buffer
        integer :: k

        buffer = windows(window%index)%buffer
        allocate (next(0))
        allocate (prev(0))

        if (allocated(windows(window%index)%next_buffers)) then
            do k = 1, size(windows(window%index)%next_buffers)
                associate (b => windows(window%index)%next_buffers(k))
                    if (.not. same_handle(b, buffer) .and. is_buffer_live(b)) next = [next, b]
                end associate
            end do
        end if

        if (allocated(windows(window%index)%prev_buffers)) then
            do k = 1, size(windows(window%index)%prev_buffers)
                associate (b => windows(window%index)%prev_buffers(k)%buffer)
                    ! Remove next-buffers from prev-buffers.
                    if (.not. same_handle(b, buffer) .and. is_buffer_live(b) .and. .not. has_buffer(next, b)) then
                        prev = [prev, b]
                    end if
                end associate
            end do
        end if

        list = [prev(size(prev):1:-1), buffer, next]
    end subroutine window_buffers

    !> Return a list of tabs that should be displayed in the tab line.
    !> By default returns a list of window buffers, i.e. buffers previously
    !> shown in the same window where the tab line is displayed.
    !> This list can be overridden by changing the value of the
    !> variable `tab_line_tabs_function'.  Port of
    !> tab-line.el:tab-line-tabs-window-buffers.
    recursive subroutine get_tab_line_tabs_window_buffers(window, tabs)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        type(handle_type), allocatable :: list(:)
        integer :: k

        call window_buffers(window, list)
        call filter_buffers(list)
        allocate (tabs(size(list)))

        do k = 1, size(list)
            tabs(k) = buffer_tab(list(k))
        end do
    end subroutine get_tab_line_tabs_window_buffers

    !> Filter LIST with `tab_line_tabs_window_buffers_filter_function'.
    !> Not a port.
    recursive subroutine filter_buffers(list)
        type(handle_type), allocatable, intent(inout) :: list(:)
        procedure(tab_line_buffer_filter_procedure), pointer :: filter

        filter => tab_line_tabs_window_buffers_filter_function
        if (associated(filter)) call filter(list)
        if (.not. allocated(list)) allocate (list(0))
    end subroutine filter_buffers

    !> Like `get_tab_line_tabs_window_buffers' but keep stable sorting order.
    !> This means that switching to a buffer previously shown in the same
    !> window will keep the same order of tabs that was before switching.
    !> And newly displayed buffers are added to the end of the tab line.
    !> Port of tab-line.el:tab-line-tabs-fixed-window-buffers.
    recursive subroutine get_tab_line_tabs_fixed_window_buffers(window, tabs)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        type(handle_type), allocatable :: list(:), old(:)
        type(handle_type) :: item
        integer, allocatable :: keys(:)
        integer :: i, j, key

        call get_tab_line_tabs_window_buffers(window, tabs)
        list = tab_buffers(tabs)

        if (allocated(windows(window%index)%tab_line%buffers)) then
            old = windows(window%index)%tab_line%buffers
        else
            allocate (old(0))
        end if

        allocate (keys(size(list)))

        do i = 1, size(list)
            keys(i) = buffer_position(old, list(i))
            if (keys(i) == 0) keys(i) = huge(0)
        end do

        ! A stable sort by the position in the old buffers.
        do i = 2, size(list)
            item = list(i)
            key = keys(i)
            j = i - 1

            do while (j >= 1)
                if (keys(j) <= key) exit
                list(j + 1) = list(j)
                keys(j + 1) = keys(j)
                j = j - 1
            end do

            list(j + 1) = item
            keys(j + 1) = key
        end do

        windows(window%index)%tab_line%buffers = list
        deallocate (tabs)
        allocate (tabs(size(list)))

        do i = 1, size(list)
            tabs(i) = buffer_tab(list(i))
        end do
    end subroutine get_tab_line_tabs_fixed_window_buffers

    !> The tabs of WINDOW: `tab_line_tabs_function's, with WINDOW's buffer
    !> current.  Not a port.
    recursive subroutine window_tabs(window, tabs)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), allocatable, intent(out) :: tabs(:)
        procedure(tab_line_tabs_procedure), pointer :: tabs_function

        tabs_function => tab_line_tabs_function

        if (associated(tabs_function)) then
            call tabs_function(window, tabs)
        else
            call get_tab_line_tabs_fixed_window_buffers(window, tabs)
        end if

        if (.not. allocated(tabs)) allocate (tabs(0))
    end subroutine window_tabs

    ! ------------------------------------------------------------------
    ! Formatting the tabs
    ! ------------------------------------------------------------------

    !> Append the segment TEXT in FACES (the first above the others) to
    !> SEGMENTS.  Not a port.
    subroutine append_segment(segments, text, faces)
        type(header_line_segment_type), allocatable, intent(inout) :: segments(:)
        character(len=*), intent(in) :: text
        character(len=40), intent(in), optional :: faces(:)
        type(header_line_segment_type) :: segment

        if (.not. allocated(segments)) allocate (segments(0))
        segment%text = text
        segment%face = ""

        if (present(faces)) then
            if (size(faces) > 0) then
                segment%face = trim(faces(1))
                allocate (segment%extra_faces(size(faces) - 1))
                segment%extra_faces(:) = faces(2:)
            end if
        end if

        segments = [segments, segment]
    end subroutine append_segment

    !> Default function to use as `tab_line_tab_name_format_function',
    !> which see.  Port of tab-line.el:tab-line-tab-name-format-default
    !> (the name shows as it is: upstream doubles its "%" for the
    !> mode-line format).
    recursive subroutine format_tab_line_tab_name_default(window, tabs, k, segments, selected)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        type(header_line_segment_type), allocatable, intent(out) :: segments(:)
        logical, intent(out) :: selected
        character(len=40), allocatable :: faces(:)
        character(len=:), allocatable :: name
        logical :: show_close

        if (tabs(k)%is_buffer) then
            selected = same_handle(tabs(k)%buffer, windows(window%index)%buffer)
            call tab_line_tab_name_function(tabs(k)%buffer, tabs, name)
        else
            selected = tabs(k)%selected
            name = ""
            if (allocated(tabs(k)%name)) name = tabs(k)%name
        end if

        if (selected) then
            if (is_mode_line_window_selected()) then
                faces = [character(len=40) :: "tab_line_tab_current"]
            else
                faces = [character(len=40) :: "tab_line_tab"]
            end if
        else
            faces = [character(len=40) :: "tab_line_tab_inactive"]
        end if

        call execute_tab_face_functions(tabs, k, selected, faces)
        call append_segment(segments, name, faces)
        show_close = (tabs(k)%is_buffer .or. tabs(k)%buffer%index /= 0 .or. tabs(k)%closes_buffer) .and. &
                     .not. tab_line_close_button_show%is_off()

        if (show_close) then
            if (selected) then
                show_close = tab_line_close_button_show%choice /= TAB_LINE_CLOSE_BUTTON_SHOW_NON_SELECTED
            else
                show_close = tab_line_close_button_show%choice /= TAB_LINE_CLOSE_BUTTON_SHOW_SELECTED
            end if
        end if

        ! The close button in the tab's face (it has no face of its own on
        ! a terminal).
        if (show_close) then
            if (tab_line_close_modified_button_show .and. is_tab_line_tab_modified(tabs(k))) then
                call append_segment(segments, tab_line_close_modified_button, faces)
            else
                call append_segment(segments, tab_line_close_button, faces)
            end if
        end if
    end subroutine format_tab_line_tab_name_default

    !> Call each of `tab_line_tab_face_functions' on FACES.  Not a port.
    recursive subroutine execute_tab_face_functions(tabs, k, selected, faces)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        logical, intent(in) :: selected
        character(len=40), allocatable, intent(inout) :: faces(:)
        procedure(tab_line_tab_face_procedure), pointer :: face_function
        integer :: i

        if (.not. allocated(tab_line_tab_face_functions%entries)) return

        do i = 1, size(tab_line_tab_face_functions%entries)
            face_function => tab_line_tab_face_functions%entries(i)%face_function
            if (associated(face_function)) call face_function(tabs, k, selected, faces)
        end do
    end subroutine execute_tab_face_functions

    !> Return FACES for tab TABS(K) with alternation.
    !> SELECTED .false. means the tab is not the selected tab.
    !> When the tab is not selected and is even-numbered (counting from
    !> 0), make FACES inherit from `tab_line_tab_inactive_alternate'.  For
    !> use in `tab_line_tab_face_functions'.  Port of
    !> tab-line.el:tab-line-tab-face-inactive-alternating.
    recursive subroutine get_tab_line_tab_face_inactive_alternating(tabs, k, selected, faces)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        logical, intent(in) :: selected
        character(len=40), allocatable, intent(inout) :: faces(:)

        if (.not. selected .and. modulo(k - 1, 2) == 0) then
            faces = [character(len=40) :: "tab_line_tab_inactive_alternate", faces]
        end if

        if (size(tabs) < 0) faces = faces(1:0)
    end subroutine get_tab_line_tab_face_inactive_alternating

    !> Return FACES for tab TABS(K) according to whether its buffer is special.
    !> The tab is either a buffer, or a tab with a buffer.
    !> When it specifies a non-file-visiting buffer, make FACES inherit
    !> from `tab_line_tab_special'.
    !> For use in `tab_line_tab_face_functions'.  Port of
    !> tab-line.el:tab-line-tab-face-special.
    recursive subroutine get_tab_line_tab_face_special(tabs, k, selected, faces)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        logical, intent(in) :: selected
        character(len=40), allocatable, intent(inout) :: faces(:)
        type(status_type) :: status
        integer(int64) :: bi

        if (tabs(k)%buffer%index == 0) return
        call resolve_buffer(tabs(k)%buffer, bi, status)
        if (.not. status%ok()) return
        if (.not. allocated(buffers(bi)%filename)) faces = [character(len=40) :: "tab_line_tab_special", faces]
        if (selected .and. size(tabs) < 0) faces = faces(1:0)
    end subroutine get_tab_line_tab_face_special

    !> Return FACES for tab TABS(K) according to whether its buffer is modified.
    !> The tab is either a buffer, or a tab with a buffer.
    !> When the tab's buffer is a modified, file-backed buffer, make FACES
    !> inherit from `tab_line_tab_modified'.
    !> For use in `tab_line_tab_face_functions'.  Port of
    !> tab-line.el:tab-line-tab-face-modified.
    recursive subroutine get_tab_line_tab_face_modified(tabs, k, selected, faces)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        logical, intent(in) :: selected
        character(len=40), allocatable, intent(inout) :: faces(:)

        if (is_tab_line_tab_modified(tabs(k))) faces = [character(len=40) :: "tab_line_tab_modified", faces]
        if (selected .and. size(tabs) < 0) faces = faces(1:0)
    end subroutine get_tab_line_tab_face_modified

    !> Return FACES for tab TABS(K) according to whether it's a group tab.
    !> For use in `tab_line_tab_face_functions'.  Port of
    !> tab-line.el:tab-line-tab-face-group.
    recursive subroutine get_tab_line_tab_face_group(tabs, k, selected, faces)
        type(tab_line_tab_type), intent(in) :: tabs(:)
        integer, intent(in) :: k
        logical, intent(in) :: selected
        character(len=40), allocatable, intent(inout) :: faces(:)

        if (tabs(k)%group_tab) faces = [character(len=40) :: "tab_line_tab_group", faces]
        if (selected .and. size(tabs) < 0) faces = faces(1:0)
    end subroutine get_tab_line_tab_face_group

    !> The separator between tabs: `tab_line_separator', else a space on a
    !> window system and "|" on a terminal.  Not a port.
    function separator_text() result(separator)
        character(len=:), allocatable :: separator

        if (allocated(tab_line_separator)) then
            separator = tab_line_separator
        else if (associated(selected_frame%rif)) then
            separator = " "
        else
            separator = "|"
        end if
    end function separator_text

    !> Template of the format for displaying tab line for WINDOW: SEGMENTS
    !> for TABS.  This is used by `format_tab_line'.  Port of
    !> tab-line.el:tab-line-format-template.
    recursive subroutine make_tab_line_format_template(window, tabs, segments)
        type(handle_type), intent(in) :: window
        type(tab_line_tab_type), intent(in) :: tabs(:)
        type(header_line_segment_type), allocatable, intent(out) :: segments(:)
        type(tab_line_string_type), allocatable :: strings(:)
        type(header_line_segment_type), allocatable :: tab_segments(:)
        type(tab_line_hscroll_type) :: hscroll
        character(len=:), allocatable :: separator
        logical :: show_arrows, selected
        integer :: k, first, n

        call load_tab_line_buttons()
        separator = separator_text()
        allocate (strings(size(tabs)))

        do k = 1, size(tabs)
            call tab_line_tab_name_format_function(window, tabs, k, tab_segments, selected)
            call append_segment(strings(k)%segments, separator)
            if (allocated(tab_segments)) strings(k)%segments = [strings(k)%segments, tab_segments]
            strings(k)%tab = k
            strings(k)%selected = selected
        end do

        hscroll = windows(window%index)%tab_line%hscroll
        call auto_hscroll(window, strings, hscroll, show_arrows)
        allocate (segments(0))
        n = size(strings)

        if (.not. show_arrows) then
            if (hscroll%tab >= 0 .or. hscroll%automatic_tab >= 0) then
                hscroll = tab_line_hscroll_type()
                windows(window%index)%tab_line%hscroll = hscroll
            end if
        else
            call append_segment(segments, separator)
            if (first_tab(hscroll) > 0) call append_segment(segments, tab_line_left_button)

            if (first_tab(hscroll) >= 0) then
                if (first_tab(hscroll) < n - 1) call append_segment(segments, tab_line_right_button)
            else if (n > 1) then
                call append_segment(segments, tab_line_right_button)
            end if
        end if

        first = max(0, first_tab(hscroll))

        do k = first + 1, n
            if (allocated(strings(k)%segments)) segments = [segments, strings(k)%segments]
        end do

        call append_segment(segments, separator)

        if (associated(tab_line_tabs_function)) then
            if (tab_line_new_button_functions%has(tab_line_tabs_function) .and. tab_line_new_button_show .and. &
                allocated(tab_line_new_button)) call append_segment(segments, tab_line_new_button)
        end if
    end subroutine make_tab_line_format_template

    !> The index from 0 of the first tab HSCROLL shows, -1 for none.
    !> Not a port.
    integer function first_tab(hscroll) result(k)
        type(tab_line_hscroll_type), intent(in) :: hscroll

        k = -1

        if (hscroll%tab >= 0) then
            k = hscroll%tab
        else if (hscroll%automatic_tab >= 0) then
            k = hscroll%automatic_tab
        end if
    end function first_tab

    ! ------------------------------------------------------------------
    ! Automatic horizontal scrolling
    ! ------------------------------------------------------------------

    !> The characters of STRINGS(FROM) to STRINGS(TO) (backward when FROM is
    !> greater): their widths, and the tab of each (0 for a separator's),
    !> each "%" of a tab's text twice (upstream inserts the text the
    !> mode-line format doubles them in).  Not a port.
    subroutine string_characters(strings, from, to, widths, owners)
        type(tab_line_string_type), intent(in) :: strings(:)
        integer, intent(in) :: from, to
        integer, allocatable, intent(out) :: widths(:), owners(:)
        integer(int64) :: i
        integer(int32) :: c, clen
        integer :: k, s, step, column, owner, copies

        allocate (widths(0))
        allocate (owners(0))
        step = 1
        if (from > to) step = -1
        column = 0

        do k = from, to, step
            do s = 1, size(strings(k)%segments)
                owner = strings(k)%tab
                if (s == 1) owner = 0
                i = 1

                associate (text => strings(k)%segments(s)%text)
                    do while (i <= len(text, kind=int64))
                        call string_char_and_length(text, i, c, clen)
                        i = i + clen
                        copies = 1
                        if (owner /= 0 .and. c == iachar("%")) copies = 2

                        do while (copies > 0)
                            widths = [widths, glyph_columns(c, column)]
                            owners = [owners, owner]
                            column = column + widths(size(widths))
                            copies = copies - 1
                        end do
                    end do
                end associate
            end do
        end do
    end subroutine string_characters

    !> Where the characters WIDTHS continue on a second screen line of
    !> WIDTH columns (whose last column a continued line keeps): the index
    !> of the first character there, 0 when they fit on one line.  Port of
    !> what tab-line.el:tab-line-auto-hscroll asks of vertical-motion.
    integer function continuation(widths, width) result(p)
        integer, intent(in) :: widths(:), width
        integer :: column

        column = 0

        do p = 1, size(widths)
            if (column + widths(p) > width - 1) return
            column = column + widths(p)
        end do

        p = 0
    end function continuation

    !> The tab whose text is at the start of the second screen line: the
    !> character P of OWNERS (a tab's index, 0 for a separator) is there;
    !> 0 for none.  Port of what tab-line.el:tab-line-auto-hscroll does with
    !> previous-single-property-change and get-pos-property at point
    !> (text properties are rear-sticky).
    integer function tab_at_continuation(owners, p) result(tab)
        integer, intent(in) :: owners(:), p
        integer :: q

        tab = 0
        q = previous_change(p)
        if (q == 0) return
        tab = owners(q - 1)
        if (tab /= 0) return
        q = previous_change(q)
        if (q == 0) return
        tab = owners(q - 1)

    contains

        !> The first character of the run of characters with the same tab
        !> that ends before character AT; 0 when that run starts the text.
        integer function previous_change(at) result(start)
            integer, intent(in) :: at

            start = 0
            if (at <= 1) return
            start = at - 1

            do while (start > 1)
                if (owners(start - 1) /= owners(at - 1)) exit
                start = start - 1
            end do

            if (start == 1) start = 0
        end function previous_change
    end function tab_at_continuation

    !> Scroll the tab line of WINDOW so that the selected one of STRINGS
    !> shows (when `tab_line_auto_hscroll' and the user did not scroll it):
    !> SHOW_ARROWS whether the strings do not fit on the line, HSCROLL the
    !> scrolling then, also set as WINDOW's `tab_line_hscroll' when it
    !> changes.  Port of tab-line.el:tab-line-auto-hscroll (the strings
    !> measured on a line as wide as WINDOW's text area).
    recursive subroutine auto_hscroll(window, strings, hscroll, show_arrows)
        type(handle_type), intent(in) :: window
        type(tab_line_string_type), intent(in) :: strings(:)
        type(tab_line_hscroll_type), intent(inout) :: hscroll
        logical, intent(out) :: show_arrows
        integer, allocatable :: widths(:), owners(:)
        integer :: width, selected, k, p

        width = window_text_cols(windows(window%index))
        call string_characters(strings, 1, size(strings), widths, owners)
        ! Continuation means tab-line doesn't fit completely, thus scroll
        ! arrows are needed for scrolling.
        show_arrows = continuation(widths, width) > 0
        ! Try to auto-hscroll only when scrolling is needed, but no manual
        ! scrolling was performed before.
        if (.not. (tab_line_auto_hscroll .and. show_arrows .and. hscroll%tab < 0)) return
        selected = 0

        do k = 1, size(strings)
            if (strings(k)%selected) then
                selected = k
                exit
            end if
        end do

        ! Do nothing if no tab is selected.
        if (selected == 0) return

        if (hscroll%automatic_tab < 0 .or. selected - 1 < hscroll%automatic_tab) then
            ! Selected is scrolled to the left, or no scrolling yet.
            call string_characters(strings, selected, 1, widths, owners)
            p = continuation(widths, width)

            if (p > 0) then
                call scroll_to(tab_at_continuation(owners, p))
            else
                hscroll = tab_line_hscroll_type()
                windows(window%index)%tab_line%hscroll = hscroll
            end if
        else
            ! Check if the selected tab is already visible.
            call string_characters(strings, hscroll%automatic_tab + 1, selected, widths, owners)

            if (continuation(widths, width) > 0) then
                ! Not visible already.
                call string_characters(strings, selected, 1, widths, owners)
                p = continuation(widths, width)
                if (p > 0) call scroll_to(tab_at_continuation(owners, p))
            end if
        end if

    contains

        !> Scroll to show TAB first (an index from 1; 0 for none).
        subroutine scroll_to(tab)
            integer, intent(in) :: tab

            if (tab == 0) return
            hscroll = tab_line_hscroll_type(automatic_tab=tab - 1)
            windows(window%index)%tab_line%hscroll = hscroll
        end subroutine scroll_to
    end subroutine auto_hscroll

    ! ------------------------------------------------------------------
    ! The tab line
    ! ------------------------------------------------------------------

    !> Whether the tabs A and B are the same (equal).  Not a port.
    logical function is_same_tab(a, b) result(same)
        type(tab_line_tab_type), intent(in) :: a, b

        same = (a%is_buffer .eqv. b%is_buffer) .and. same_handle(a%buffer, b%buffer) .and. &
               (a%selected .eqv. b%selected) .and. (a%group_tab .eqv. b%group_tab) .and. &
               (a%closes_buffer .eqv. b%closes_buffer) .and. is_same_group(a%name, b%name) .and. &
               is_same_group(a%group, b%group)
    end function is_same_tab

    !> The index of WINDOW's entry in the caches, 0 for none.  Not a port.
    integer function cache_index(window) result(k)
        type(handle_type), intent(in) :: window

        k = 0
        if (.not. allocated(tab_line_caches)) return

        do k = 1, size(tab_line_caches)
            if (same_handle(tab_line_caches(k)%window, window)) return
        end do

        k = 0
    end function cache_index

    !> Forget the tab line made last for WINDOW (its `tab_line_cache'
    !> parameter).  Not a port.
    subroutine forget_cache(window)
        type(handle_type), intent(in) :: window
        type(tab_line_cache_type), allocatable :: kept(:)
        integer :: k, j

        k = cache_index(window)
        if (k == 0) return
        allocate (kept(size(tab_line_caches) - 1))

        do j = 1, size(tab_line_caches)
            if (j < k) kept(j) = tab_line_caches(j)
            if (j > k) kept(j - 1) = tab_line_caches(j)
        end do

        call move_alloc(kept, tab_line_caches)
    end subroutine forget_cache

    !> Force redisplay of the current buffer's tab line.
    !> This procedure also clears the tab-line cache.  With ALL true,
    !> it clears the tab-line cache of all tab lines and forces their redisplay.
    !> Port of tab-line.el:tab-line-force-update.
    subroutine force_tab_line_update(all)
        logical, intent(in) :: all

        if (all) then
            if (allocated(tab_line_caches)) deallocate (tab_line_caches)
        else
            call forget_cache(selected_window())
        end if

        call force_mode_line_update(all)
    end subroutine force_tab_line_update

    !> Format for displaying the tab line of WINDOW (upstream the selected
    !> window), whose buffer is current: SEGMENTS.  The procedure of the
    !> `tab_line_format' that `tab_line_mode' sets.  Port of
    !> tab-line.el:tab-line-format (with tab-line-cache-key-default; the
    !> function variable tab-line-cache-key-function is Deferred).
    recursive subroutine format_tab_line(window, segments)
        type(handle_type), intent(in) :: window
        type(header_line_segment_type), allocatable, intent(out) :: segments(:)
        type(tab_line_cache_type) :: key
        type(tab_line_cache_type), allocatable :: kept(:)
        type(status_type) :: status
        integer :: k, j
        logical :: same

        if (.not. is_window_live(window)) return
        call window_tabs(window, key%tabs)
        key%window = window
        key%buffer_name = name_of(windows(window%index)%buffer)
        key%hscroll = windows(window%index)%tab_line%hscroll
        key%selected = is_mode_line_window_selected()
        key%modified = .false.

        if (tab_line_close_modified_button_show .or. &
            tab_line_tab_face_functions%has(get_tab_line_tab_face_modified)) then
            if (allocated(buffers(current)%filename)) then
                call is_buffer_modified(current_buffer(), key%modified, status)
                if (.not. status%ok()) key%modified = .false.
            end if
        end if

        k = cache_index(window)

        ! Enable auto-hscroll again after it was disabled on manual scrolling.
        ! The moment to enable it is when the window-buffer was updated.
        if (tab_line_auto_hscroll .and. key%hscroll%tab >= 0 .and. k > 0) then
            if (tab_line_caches(k)%buffer_name /= key%buffer_name .or. &
                len(tab_line_caches(k)%buffer_name) /= len(key%buffer_name)) then
                windows(window%index)%tab_line%hscroll = tab_line_hscroll_type(automatic_tab=key%hscroll%tab)
            end if
        end if

        if (k > 0) then
            same = is_same_key(tab_line_caches(k), key)

            if (same) then
                if (allocated(tab_line_caches(k)%segments)) segments = tab_line_caches(k)%segments
                return
            end if
        end if

        call make_tab_line_format_template(window, key%tabs, segments)
        if (allocated(segments)) key%segments = segments
        call forget_cache(window)
        if (.not. allocated(tab_line_caches)) allocate (tab_line_caches(0))
        allocate (kept(size(tab_line_caches) + 1))
        j = 0

        ! Keep the entries of live windows.
        do k = 1, size(tab_line_caches)
            if (.not. is_window_live(tab_line_caches(k)%window)) cycle
            j = j + 1
            kept(j) = tab_line_caches(k)
        end do

        j = j + 1
        kept(j) = key
        tab_line_caches = kept(1:j)
    end subroutine format_tab_line

    !> Whether the cache keys of A and B are the same.  Not a port.
    logical function is_same_key(a, b) result(same)
        type(tab_line_cache_type), intent(in) :: a, b
        integer :: k

        same = .false.
        if (.not. (allocated(a%tabs) .and. allocated(b%tabs))) return
        if (size(a%tabs) /= size(b%tabs)) return

        do k = 1, size(a%tabs)
            if (.not. is_same_tab(a%tabs(k), b%tabs(k))) return
        end do

        if (a%buffer_name /= b%buffer_name .or. len(a%buffer_name) /= len(b%buffer_name)) return
        if (a%hscroll%tab /= b%hscroll%tab .or. a%hscroll%automatic_tab /= b%hscroll%automatic_tab) return
        if (a%selected .neqv. b%selected) return
        same = a%modified .eqv. b%modified
    end function is_same_key

    !> Load the icons for the tab line's buttons when they are first used:
    !> on a terminal, their text (the close button for a modified buffer
    !> is a bullet where the terminal can show it).  Port of tab-line.el's
    !> define-icon of tab-line-new, tab-line-close, tab-line-close-modified,
    !> tab-line-left and tab-line-right with icons.el:icon-string (the
    !> images wait for the GUI).
    subroutine load_tab_line_buttons()
        if (.not. allocated(tab_line_new_button)) tab_line_new_button = " + "
        if (.not. allocated(tab_line_close_button)) tab_line_close_button = " x"
        if (.not. allocated(tab_line_left_button)) tab_line_left_button = " <"
        if (.not. allocated(tab_line_right_button)) tab_line_right_button = "> "

        if (.not. allocated(tab_line_close_modified_button)) then
            ! mule.el:char-displayable-p of the bullet on a terminal: its
            ! coding system can encode it.
            if (is_char_encodable(int(z'2022'), terminal_coding_system())) then
                tab_line_close_modified_button = " "//char(226)//char(128)//char(162)
            else
                tab_line_close_modified_button = " *"
            end if
        end if
    end subroutine load_tab_line_buttons

    ! ------------------------------------------------------------------
    ! Scrolling, switching and moving tabs
    ! ------------------------------------------------------------------

    !> Scroll the tab line of WINDOW (absent: the selected window) ARG
    !> positions to the right (to the left when negative).  Port of
    !> tab-line.el:tab-line-hscroll.
    recursive subroutine scroll_tab_line(arg, window)
        integer, intent(in) :: arg
        type(handle_type), intent(in), optional :: window
        type(tab_line_tab_type), allocatable :: tabs(:)
        type(handle_type) :: w
        integer :: first

        if (present(window)) then
            w = window
        else
            w = selected_window()
        end if

        call window_tabs(w, tabs)
        first = max(0, first_tab(windows(w%index)%tab_line%hscroll))
        windows(w%index)%tab_line%hscroll = tab_line_hscroll_type(tab=max(0, min(first + arg, size(tabs) - 1)))
        if (present(window)) call force_mode_line_update(.true.)
    end subroutine scroll_tab_line

    !> Scroll the tab line ARG positions to the right or left
    !> (prefix argument ARG, which must be a number when given: a
    !> `wrong_type_argument' error condition occurs otherwise).  Not a port.
    recursive subroutine scroll_tab_line_by_prefix(direction, status)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status
        integer :: arg

        status = status_type()
        arg = 1

        if (current_prefix_arg%kind == PREFIX_NUMBER) then
            arg = int(current_prefix_arg%value)
        else if (current_prefix_arg%kind /= PREFIX_NONE) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "number_or_marker_p, "//prefix_arg_text(current_prefix_arg))
            return
        end if

        call scroll_tab_line(direction*arg, selected_window())
        call force_mode_line_update(.false.)
    end subroutine scroll_tab_line_by_prefix

    !> Scroll the tab line ARG positions to the right.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-line.el:tab-line-hscroll-right (the command, from the
    !> keyboard: the selected window).
    recursive subroutine execute_scroll_tab_line_right(status)
        type(status_type), intent(out) :: status

        call scroll_tab_line_by_prefix(1, status)
    end subroutine execute_scroll_tab_line_right

    !> Scroll the tab line ARG positions to the left.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-line.el:tab-line-hscroll-left (the command, from the
    !> keyboard: the selected window).
    recursive subroutine execute_scroll_tab_line_left(status)
        type(status_type), intent(out) :: status

        call scroll_tab_line_by_prefix(-1, status)
    end subroutine execute_scroll_tab_line_left

    !> Switch to BUFFER in WINDOW (absent: the selected one), keeping the
    !> original order of prev/next buffers when the tabs are the window
    !> buffers.  Port of tab-line.el:tab-line-select-tab-buffer.
    recursive subroutine select_tab_line_tab_buffer(buffer, status, window)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: window
        type(handle_type), allocatable :: next(:), prev(:)
        type(handle_type) :: w, b, saved
        type(status_type) :: st
        integer :: k, n

        status = status_type()

        if (present(window)) then
            w = window
        else
            w = selected_window()
        end if

        if (associated(tab_line_tabs_function, get_tab_line_tabs_window_buffers)) then
            b = windows(w%index)%buffer
            allocate (next(0))
            allocate (prev(0))

            if (allocated(windows(w%index)%next_buffers)) then
                do k = 1, size(windows(w%index)%next_buffers)
                    if (.not. same_handle(windows(w%index)%next_buffers(k), b)) next = [next, windows(w%index)%next_buffers(k)]
                end do
            end if

            if (allocated(windows(w%index)%prev_buffers)) then
                do k = 1, size(windows(w%index)%prev_buffers)
                    associate (p => windows(w%index)%prev_buffers(k)%buffer)
                        if (.not. same_handle(p, b) .and. .not. has_buffer(next, p)) prev = [prev, p]
                    end associate
                end do
            end if

            n = buffer_position(next, buffer)

            if (n > 0) then
                do k = 1, n
                    b = switch_to_next_buffer(w, status)
                    if (.not. status%ok()) return
                end do

                return
            end if

            n = buffer_position(prev, buffer)

            do k = 1, n
                b = switch_to_prev_buffer(w, BK_NONE, status)
                if (.not. status%ok()) return
            end do
        else
            ! with-selected-window.
            saved = selected_window()
            if (.not. same_window(w, saved)) call select_window(w, status, norecord=.true.)
            if (status%ok()) call switch_to_buffer(name_of(buffer), status)
            if (.not. same_window(w, saved) .and. is_window_live(saved)) call select_window(saved, st, norecord=.true.)
        end if
    end subroutine select_tab_line_tab_buffer

    !> Switch to the ARGth previous (or with NEXT next) tab's buffer.
    !> Port of tab-line.el:tab-line-switch-to-prev-tab and
    !> tab-line.el:tab-line-switch-to-next-tab (their common part).
    recursive subroutine switch_tab(next, arg, status)
        logical, intent(in) :: next
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(tab_line_tab_type), allocatable :: tabs(:)
        type(handle_type), allocatable :: list(:)
        integer(int64) :: old_pos, new_pos, n

        status = status_type()

        if (associated(tab_line_tabs_function, get_tab_line_tabs_window_buffers)) then
            if (next) then
                call next_buffer(status, arg, .true.)
            else
                call previous_buffer(status, arg, .true.)
            end if

            return
        end if

        call window_tabs(selected_window(), tabs)
        list = tab_buffers(tabs)
        n = size(list)
        old_pos = buffer_position(list, current_buffer())
        if (old_pos == 0) return

        if (next) then
            new_pos = old_pos - 1 + arg
        else
            new_pos = old_pos - 1 - arg
        end if

        if (tab_line_switch_cycling) then
            new_pos = modulo(new_pos, n)
        else if (next) then
            new_pos = min(new_pos, n - 1)
        else
            new_pos = max(new_pos, 0_int64)
        end if

        if (new_pos < 0 .or. new_pos >= n) return
        call switch_to_buffer(name_of(list(new_pos + 1)), status)
    end subroutine switch_tab

    !> Switch to the ARGth previous tab's buffer.
    !> When `tab_line_tabs_function' is `get_tab_line_tabs_window_buffers',
    !> its effect is the same as using the `previous_buffer' command
    !> (\[previous_buffer]).
    !> For other values of `tab_line_tabs_function' this command
    !> switches to the previous buffer in the sequence defined by
    !> `tab_line_tabs_function'.  To wrap buffer cycling in this case
    !> is possible when `tab_line_switch_cycling' is true.  Port of
    !> tab-line.el:tab-line-switch-to-prev-tab (from the keyboard: the
    !> selected window).
    recursive subroutine switch_to_tab_line_previous_tab(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        call switch_tab(.false., n, status)
    end subroutine switch_to_tab_line_previous_tab

    !> Switch to the next ARGth tab's buffer.
    !> When `tab_line_tabs_function' is `get_tab_line_tabs_window_buffers',
    !> its effect is the same as using the `next_buffer' command
    !> (\[next_buffer]).
    !> For other values of `tab_line_tabs_function' this command
    !> switches to the next buffer in the sequence defined by
    !> `tab_line_tabs_function'.  To wrap buffer cycling in this case
    !> is possible when `tab_line_switch_cycling' is true.  Port of
    !> tab-line.el:tab-line-switch-to-next-tab (from the keyboard: the
    !> selected window).
    recursive subroutine switch_to_tab_line_next_tab(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        call switch_tab(.true., n, status)
    end subroutine switch_to_tab_line_next_tab

    !> Switch to the ARGth previous tab's buffer (the command).  Port of
    !> tab-line.el:tab-line-switch-to-prev-tab (the command).
    recursive subroutine execute_switch_to_tab_line_previous_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_tab_line_previous_tab(status, prefix_numeric_value(current_prefix_arg))
    end subroutine execute_switch_to_tab_line_previous_tab

    !> Switch to the next ARGth tab's buffer (the command).  Port of
    !> tab-line.el:tab-line-switch-to-next-tab (the command).
    recursive subroutine execute_switch_to_tab_line_next_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_tab_line_next_tab(status, prefix_numeric_value(current_prefix_arg))
    end subroutine execute_switch_to_tab_line_next_tab

    !> Move a tab to a different position on the tab line.
    !> ARG specifies the number of positions to move:
    !> - When positive, move the current tab ARG positions to the right.
    !> - When negative, move the current tab -ARG positions to the left.
    !> - When absent, act as if ARG is 1, moving one position to the right.
    !> It can be used only when `tab_line_tabs_function' is
    !> customized to `get_tab_line_tabs_fixed_window_buffers'.  Port of
    !> tab-line.el:tab-line-move-tab-forward.
    subroutine move_tab_line_tab_forward(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type), allocatable :: list(:), others(:)
        type(handle_type) :: window, buffer
        integer(int64) :: pos, n, new_pos
        integer :: k

        status = status_type()
        if (.not. associated(tab_line_tabs_function, get_tab_line_tabs_fixed_window_buffers)) return
        window = selected_window()

        if (allocated(windows(window%index)%tab_line%buffers)) then
            list = windows(window%index)%tab_line%buffers
        else
            allocate (list(0))
        end if

        buffer = current_buffer()
        pos = buffer_position(list, buffer)
        n = size(list)

        if (pos == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "number_or_marker_p, .false.")
            return
        end if

        if (present(arg)) then
            new_pos = pos - 1 + arg
        else
            new_pos = pos
        end if

        if (n <= 1) return

        if (tab_line_switch_cycling) then
            new_pos = modulo(new_pos, n)
        else
            new_pos = max(0_int64, min(new_pos, n - 1))
        end if

        allocate (others(0))

        do k = 1, size(list)
            if (.not. same_handle(list(k), buffer)) others = [others, list(k)]
        end do

        new_pos = min(new_pos, int(size(others), int64))
        windows(window%index)%tab_line%buffers = [others(1:new_pos), buffer, others(new_pos + 1:)]
        call forget_cache(window)
        call force_mode_line_update(.false.)
    end subroutine move_tab_line_tab_forward

    !> Move a tab to a different position on the tab line.
    !> ARG specifies the number of positions to move:
    !> - When positive, move the current tab ARG positions to the left.
    !> - When negative, move the current tab -ARG positions to the right.
    !> - When absent, act as if ARG is 1, moving one position to the left.
    !> It can be used only when `tab_line_tabs_function' is
    !> customized to `get_tab_line_tabs_fixed_window_buffers'.  Port of
    !> tab-line.el:tab-line-move-tab-backward.
    subroutine move_tab_line_tab_backward(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        if (present(arg)) then
            call move_tab_line_tab_forward(status, -arg)
        else
            call move_tab_line_tab_forward(status, -1_int64)
        end if
    end subroutine move_tab_line_tab_backward

    !> Move the current tab ARG positions to the right (the command).
    !> Port of tab-line.el:tab-line-move-tab-forward (the command).
    subroutine execute_move_tab_line_tab_forward(status)
        type(status_type), intent(out) :: status

        call move_tab_line_tab_forward(status, prefix_numeric_value(current_prefix_arg))
    end subroutine execute_move_tab_line_tab_forward

    !> Move the current tab ARG positions to the left (the command).
    !> Port of tab-line.el:tab-line-move-tab-backward (the command).
    subroutine execute_move_tab_line_tab_backward(status)
        type(status_type), intent(out) :: status

        call move_tab_line_tab_backward(status, prefix_numeric_value(current_prefix_arg))
    end subroutine execute_move_tab_line_tab_backward

    ! ------------------------------------------------------------------
    ! Closing tabs
    ! ------------------------------------------------------------------

    !> The index of the current tab among TABS: the first that is the
    !> current buffer or has it; 0 for none.  Port of
    !> tab-line.el:tab-line--current-tab (as an index).
    integer function current_tab(tabs) result(k)
        type(tab_line_tab_type), intent(in) :: tabs(:)

        do k = 1, size(tabs)
            if (same_handle(tabs(k)%buffer, current_buffer())) return
        end do

        k = 0
    end function current_tab

    !> Close TAB as `close_tab_line_tab' does (a tab with no buffer and no
    !> name for none).  Not a port.
    recursive subroutine close_one_tab(tab, status)
        type(tab_line_tab_type), intent(in) :: tab
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window

        status = status_type()
        buffer = tab%buffer

        if (.not. tab%is_buffer .and. tab%closes_buffer) then
            call close_buffer(buffer, status)
        else if (associated(tab_line_close_tab_function%procedure)) then
            call tab_line_close_tab_function%procedure(tab, status)
        else if (tab_line_close_tab_function%choice == TAB_LINE_CLOSE_TAB_FUNCTION_CLOSE_BUFFER) then
            ! (kill-buffer nil) kills the current buffer.
            if (buffer%index == 0) buffer = current_buffer()
            call close_buffer(buffer, status)
        else if (tab_line_close_tab_function%choice == TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER) then
            if (same_handle(buffer, current_buffer())) then
                call bury_buffer(status)
            else if (buffer%index /= 0) then
                window = selected_window()
                call remove_prev_buffer(window, buffer)
                call remove_next_buffer(window, buffer)
            end if
        end if
    end subroutine close_one_tab

    !> Remove BUFFER from the previous buffers of WINDOW,
    !> freeing their markers.  Not a port.
    subroutine remove_prev_buffer(window, buffer)
        type(handle_type), intent(in) :: window, buffer
        type(prev_buffer_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(windows(window%index)%prev_buffers)) return
        allocate (kept(0))

        do k = 1, size(windows(window%index)%prev_buffers)
            if (same_handle(windows(window%index)%prev_buffers(k)%buffer, buffer)) then
                call destroy_previous_buffer(windows(window%index)%prev_buffers(k))
            else
                kept = [kept, windows(window%index)%prev_buffers(k)]
            end if
        end do

        call move_alloc(kept, windows(window%index)%prev_buffers)
    end subroutine remove_prev_buffer

    !> Remove BUFFER from the next buffers of WINDOW.  Not a port.
    subroutine remove_next_buffer(window, buffer)
        type(handle_type), intent(in) :: window, buffer
        type(handle_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(windows(window%index)%next_buffers)) return
        allocate (kept(0))

        do k = 1, size(windows(window%index)%next_buffers)
            if (.not. same_handle(windows(window%index)%next_buffers(k), buffer)) then
                kept = [kept, windows(window%index)%next_buffers(k)]
            end if
        end do

        call move_alloc(kept, windows(window%index)%next_buffers)
    end subroutine remove_next_buffer

    !> Close the selected tab.
    !> This command is usually invoked by clicking on the close button on the
    !> right side of the tab.  This command buries the buffer, so it goes out of
    !> sight of the tab line.  Port of tab-line.el:tab-line-close-tab (from
    !> the keyboard: the current tab of the selected window).
    recursive subroutine close_tab_line_tab(status)
        type(status_type), intent(out) :: status
        type(tab_line_tab_type), allocatable :: tabs(:)
        type(tab_line_tab_type) :: tab
        integer :: k

        call window_tabs(selected_window(), tabs)
        k = current_tab(tabs)
        if (k > 0) tab = tabs(k)
        call close_one_tab(tab, status)
        call force_mode_line_update(.false.)
    end subroutine close_tab_line_tab

    !> Close all tabs on the selected window, except the current tab.
    !> It preforms the same actions on the closed tabs as in
    !> `close_tab_line_tab'.  Port of tab-line.el:tab-line-close-other-tabs
    !> (from the keyboard: the current tab of the selected window).
    recursive subroutine close_tab_line_other_tabs(status)
        type(status_type), intent(out) :: status
        type(tab_line_tab_type), allocatable :: tabs(:)
        type(tab_line_tab_type) :: keep
        integer :: k

        status = status_type()
        call window_tabs(selected_window(), tabs)
        k = current_tab(tabs)
        if (k > 0) keep = tabs(k)

        do k = 1, size(tabs)
            if (is_same_tab(tabs(k), keep)) cycle
            call close_one_tab(tabs(k), status)
            if (.not. status%ok()) return
        end do

        call force_mode_line_update(.false.)
    end subroutine close_tab_line_other_tabs

    !> Close the selected tab (the command).  Port of
    !> tab-line.el:tab-line-close-tab (the command).
    recursive subroutine execute_close_tab_line_tab(status)
        type(status_type), intent(out) :: status

        call close_tab_line_tab(status)
    end subroutine execute_close_tab_line_tab

    !> Close all tabs on the selected window, except the current tab (the
    !> command).  Port of tab-line.el:tab-line-close-other-tabs (the
    !> command).
    recursive subroutine execute_close_tab_line_other_tabs(status)
        type(status_type), intent(out) :: status

        call close_tab_line_other_tabs(status)
    end subroutine execute_close_tab_line_other_tabs

    ! ------------------------------------------------------------------
    ! Keys
    ! ------------------------------------------------------------------

    !> The command KEY is bound to in the C-x map; null for none.
    !> Not a port.
    function ctl_x_binding(key) result(command)
        character(len=*), intent(in) :: key
        type(command_type), pointer :: command
        type(value_type), allocatable :: events(:)
        type(value_type) :: binding
        type(status_type) :: status

        command => null()
        if (.not. is_keymap_live(ctl_x_map)) return
        call key_parse(key, events, status)
        if (.not. status%ok()) return
        call lookup_key(make_keymap_ref(ctl_x_map), events, binding, status)
        if (.not. status%ok()) return
        if (vtag(binding) /= V_NAME) return
        command => find_command(trim(symbols(xsymbol(binding))%name))
    end function ctl_x_binding

    !> Remove the binding of KEY in the C-x map (no binding).
    !> Not a port.
    subroutine unset_ctl_x_key(key)
        character(len=*), intent(in) :: key
        type(value_type), allocatable :: events(:)
        type(status_type) :: status

        call key_parse(key, events, status)
        if (.not. status%ok()) return
        call define_key(ctl_x_map, events, value_type(), status)
    end subroutine unset_ctl_x_key

    !> Install key bindings to switch between tabs if so configured.
    !> Port of tab-line.el:tab-line--define-keys.
    subroutine define_tab_line_keys()
        type(status_type) :: status

        if (.not. tab_line_define_keys) return

        if (associated(ctl_x_binding("<left>"), commands%previous_buffer)) then
            call keymap_set(ctl_x_map, "<left>", "switch_to_tab_line_previous_tab", status)
        end if

        if (associated(ctl_x_binding("C-<left>"), commands%previous_buffer)) then
            call keymap_set(ctl_x_map, "C-<left>", "switch_to_tab_line_previous_tab", status)
        end if

        if (.not. associated(ctl_x_binding("M-<left>"))) then
            call keymap_set(ctl_x_map, "M-<left>", "move_tab_line_tab_backward", status)
        end if

        if (associated(ctl_x_binding("<right>"), commands%next_buffer)) then
            call keymap_set(ctl_x_map, "<right>", "switch_to_tab_line_next_tab", status)
        end if

        if (associated(ctl_x_binding("C-<right>"), commands%next_buffer)) then
            call keymap_set(ctl_x_map, "C-<right>", "switch_to_tab_line_next_tab", status)
        end if

        if (.not. associated(ctl_x_binding("M-<right>"))) then
            call keymap_set(ctl_x_map, "M-<right>", "move_tab_line_tab_forward", status)
        end if
    end subroutine define_tab_line_keys

    !> Uninstall key bindings previously bound by `define_tab_line_keys'.
    !> Port of tab-line.el:tab-line--undefine-keys.
    subroutine undefine_tab_line_keys()
        type(status_type) :: status

        if (.not. tab_line_define_keys) return

        if (associated(ctl_x_binding("<left>"), commands%switch_to_tab_line_previous_tab)) then
            call keymap_set(ctl_x_map, "<left>", "previous_buffer", status)
        end if

        if (associated(ctl_x_binding("C-<left>"), commands%switch_to_tab_line_previous_tab)) then
            call keymap_set(ctl_x_map, "C-<left>", "previous_buffer", status)
        end if

        if (associated(ctl_x_binding("M-<left>"), commands%move_tab_line_tab_backward)) call unset_ctl_x_key("M-<left>")

        if (associated(ctl_x_binding("<right>"), commands%switch_to_tab_line_next_tab)) then
            call keymap_set(ctl_x_map, "<right>", "next_buffer", status)
        end if

        if (associated(ctl_x_binding("C-<right>"), commands%switch_to_tab_line_next_tab)) then
            call keymap_set(ctl_x_map, "C-<right>", "next_buffer", status)
        end if

        if (associated(ctl_x_binding("M-<right>"), commands%move_tab_line_tab_forward)) call unset_ctl_x_key("M-<right>")
    end subroutine undefine_tab_line_keys

    ! ------------------------------------------------------------------
    ! Tab Line mode
    ! ------------------------------------------------------------------

    !> Toggle display of tab line in the windows displaying the current buffer.
    !>
    !> When this mode is enabled, each window displays a tab line on its
    !> top screen line.  The tab line is a row of tabs -- buttons which
    !> you can click to have the window display the buffer whose name is
    !> shown on the button.  Clicking on the "x" icon of the button
    !> removes the button (but does not kill the corresponding buffer).
    !> In addition, the tab line shows a "+" button which adds a new
    !> button, so you could have one more buffer shown on the tab line.
    !> Port of tab-line.el:tab-line-mode.
    recursive subroutine tab_line_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%tab_line_mode%command(status)
    end subroutine tab_line_mode

    !> The body of Tab Line mode with its new state ENABLE: set the
    !> buffer's `tab_line_format' to `format_tab_line', or clear it,
    !> unless it was set outside of the mode.  Port of
    !> tab-line.el:tab-line-mode (its body).
    recursive subroutine tab_line_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call load_tab_line_buttons()

        ! Preserve the existing tab-line set outside of this mode.
        associate (format => buffers(current)%tab_line_format)
            if (.not. format%is_set() .or. (associated(format%procedure, format_tab_line) .and. &
                                            .not. allocated(format%segments))) then
                call format%clear()
                if (enable) format%procedure => format_tab_line
            else
                call message("tab_line_format set outside of tab_line_mode, currently `"//format_text(format)//"'")
            end if
        end associate

    contains

        !> The tab line format as Fortran: its segments' texts, or a procedure.
        function format_text(format) result(text)
            type(header_line_format_type), intent(in) :: format
            character(len=:), allocatable :: text
            integer :: k

            if (associated(format%procedure)) then
                text = "a procedure set elsewhere"
                return
            end if

            text = "["

            do k = 1, size(format%segments)
                if (k > 1) text = text//", "
                text = text//'"'//format%segments(k)%text//'"'
            end do

            text = text//"]"
        end function format_text
    end subroutine tab_line_mode_body

    !> Turn on `tab_line_mode' in the current buffer if it is pertinent.
    !> Temporary buffers, buffers whose names begin with a space, buffers
    !> under major modes that are either mentioned in `tab_line_exclude_modes'
    !> or have a true `tab_line_exclude' component in their mode object,
    !> and buffers that have a true buffer-local value
    !> of `tab_line_exclude', are exempt from `tab_line_mode'.  Port of
    !> tab-line.el:tab-line-mode--turn-on (`tab-line-exclude-buffers' is
    !> Deferred).
    recursive subroutine turn_on_tab_line_mode(status)
        type(status_type), intent(out) :: status
        type(major_mode_type), pointer :: mode
        character(len=:), allocatable :: name

        status = status_type()
        if (current == 0) return
        if (is_minibuffer()) return
        name = name_of(current_buffer())

        if (len(name) > 0) then
            if (name(1:1) == " ") return
        end if

        if (is_excluded_mode(major_mode_of(current))) return
        mode => major_mode_named(major_mode_of(current))

        if (associated(mode)) then
            if (mode%tab_line_exclude) return
        end if

        if (tab_line_exclude%value) return
        call minor_modes%tab_line_mode%enable(status)
    end subroutine turn_on_tab_line_mode

    !> Toggle Tab-Line mode in many buffers.
    !> Specifically, Tab-Line mode is enabled in all buffers where
    !> `turn_on_tab_line_mode' would do it.
    !>
    !> With prefix ARG, enable Global Tab-Line mode if ARG is positive;
    !> otherwise, disable it.
    !>
    !> See `tab_line_mode' for more information on Tab-Line mode.
    !> Port of tab-line.el:global-tab-line-mode.
    recursive subroutine global_tab_line_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%global_tab_line_mode%command(status)
    end subroutine global_tab_line_mode

    !> The body of Global Tab-Line mode with its new state ON: set up the
    !> hook to handle future mode changes and new buffers, go through the
    !> existing buffers, then define or undefine the keys.  Port of
    !> tab-line.el:global-tab-line-mode (the body
    !> define-globalized-minor-mode gives it, and its own).
    recursive subroutine global_tab_line_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        integer(int64) :: b, saved
        integer :: k

        status = status_type()

        ! Setup hook to handle future mode changes and new buffers.
        if (on) then
            call after_change_major_mode_hook%prepend(enable_tab_line_mode_in_buffer)
        else
            call after_change_major_mode_hook%remove(enable_tab_line_mode_in_buffer)
        end if

        all = buffer_list()
        saved = current

        ! Go through existing buffers.
        do k = 1, size(all)
            call resolve_buffer(all(k), b, status)

            if (.not. status%ok()) then
                status = status_type()
                cycle
            end if

            call set_buffer_internal(b)

            if (on) then
                call turn_on_tab_line_mode(status)
            else if (minor_modes%tab_line_mode%enabled()) then
                call minor_modes%tab_line_mode%disable(status)
            end if

            if (.not. status%ok()) exit
        end do

        if (saved > 0 .and. saved /= current) call set_buffer_internal(saved)
        if (.not. status%ok()) return

        if (on) then
            call define_tab_line_keys()
        else
            call undefine_tab_line_keys()
        end if
    end subroutine global_tab_line_mode_body

    !> Turn on Tab Line mode in the current buffer unless it was set
    !> explicitly there.  Port of the global-tab-line-mode-enable-in-buffer
    !> define-globalized-minor-mode defines.
    recursive subroutine enable_tab_line_mode_in_buffer(status)
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()
        if (tab_line_mode_set_explicitly%value) return
        ! We are not part of the major mode hook so we don't want to set
        ! MODE-set-explicitly to true.
        saved = suppress_set_explicitly
        suppress_set_explicitly = .true.
        call turn_on_tab_line_mode(status)
        suppress_set_explicitly = saved
    end subroutine enable_tab_line_mode_in_buffer

    !> Record that Tab Line mode was set explicitly in the current buffer
    !> (it runs from `tab_line_mode_hook').  Port of the
    !> tab-line-mode-set-explicitly define-globalized-minor-mode defines.
    recursive subroutine set_tab_line_mode_explicitly(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (suppress_set_explicitly) return
        call tab_line_mode_set_explicitly%set_local(.true.)
    end subroutine set_tab_line_mode_explicitly

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define the tab line's faces, modes, commands and keymaps, as loading
    !> tab-line.el does; CTL_X_MAP is the global C-x map, whose keys
    !> `global_tab_line_mode' changes.  Not a port.
    subroutine initialize_tab_line(ctl_x_keymap)
        type(handle_type), intent(in) :: ctl_x_keymap
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK
        type(face_attributes_type) :: attrs
        type(status_type) :: st

        ctl_x_map = ctl_x_keymap
        call tab_line_tab%define("tab_line_tab", "Tab line face for selected tab.")
        call tab_line_tab%spec_entry(display_when(is_default=.true.), face_attrs(inherit="tab_line"))
        attrs = face_attrs()
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_line_tab%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), attrs)
        attrs = face_attrs(background="grey40", foreground="white")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_line_tab%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), attrs)
        call tab_line_tab%spec_entry(display_when(), face_attrs(inverse_video=.false.))
        call tab_line_tab_inactive%define("tab_line_tab_inactive", "Tab line face for non-selected tab.")
        call tab_line_tab_inactive%spec_entry(display_when(is_default=.true.), face_attrs(inherit="tab_line_tab"))
        call tab_line_tab_inactive%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                              face_attrs(background="grey75"))
        call tab_line_tab_inactive%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                              face_attrs(background="grey20"))
        call tab_line_tab_inactive%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call tab_line_tab_inactive_alternate%define("tab_line_tab_inactive_alternate", &
                                                    "Alternate face for inactive tab-line tabs."//achar(10)// &
                                                    "Applied to alternating tabs when option"//achar(10)// &
                                                    "`tab_line_tab_face_functions' includes procedure"//achar(10)// &
                                                    "`get_tab_line_tab_face_inactive_alternating'.")
        call tab_line_tab_inactive_alternate%spec_entry(display_when(), &
                                                        face_attrs(inherit="tab_line_tab_inactive", background="grey65"))
        call tab_line_tab_special%define("tab_line_tab_special", "Face for special (i.e. non-file-backed) tabs."// &
                                         achar(10)//"Applied when option `tab_line_tab_face_functions' includes"// &
                                         achar(10)//"procedure `get_tab_line_tab_face_special'.")
        call tab_line_tab_special%spec_entry(display_when(is_default=.true.), face_attrs(weight="bold"))
        call tab_line_tab_special%spec_entry(display_when(), face_attrs(slant="italic", weight="normal"), &
                                             supports=face_attrs(slant="italic"))
        call tab_line_tab_modified%define("tab_line_tab_modified", "Face for modified tabs."//achar(10)// &
                                          "Applied when option `tab_line_tab_face_functions' includes"//achar(10)// &
                                          "procedure `get_tab_line_tab_face_modified'.")
        call tab_line_tab_modified%spec_entry(display_when(), face_attrs(inherit="font_lock_doc_face"))
        call tab_line_tab_group%define("tab_line_tab_group", "Face for group tabs."//achar(10)// &
                                       "Applied when option `tab_line_tab_face_functions' includes"//achar(10)// &
                                       "procedure `get_tab_line_tab_face_group'.")
        call tab_line_tab_group%spec_entry(display_when(), face_attrs(inherit="tab_line", box=.false.))
        call tab_line_tab_current%define("tab_line_tab_current", &
                                         "Tab line face for tab with current buffer in selected window.")
        call tab_line_tab_current%spec_entry(display_when(is_default=.true.), face_attrs(inherit="tab_line_tab"))
        call tab_line_tab_current%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                             face_attrs(background="grey85"))
        call tab_line_tab_current%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                             face_attrs(background="grey40"))
        call tab_line_highlight%define("tab_line_highlight", "Tab line face for highlighting.")
        attrs = face_attrs(background="grey85", foreground="black")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_line_highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), attrs)
        attrs = face_attrs(background="grey40", foreground="white")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_line_highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), attrs)
        call tab_line_highlight%spec_entry(display_when(), face_attrs(inverse_video=.false.))
        call tab_line_close_highlight%define("tab_line_close_highlight", &
                                             "Tab line face for highlighting of the close button.")
        call tab_line_close_highlight%spec_entry(display_when(), face_attrs(foreground="red"))

        ! The settings' procedures.
        if (.not. allocated(tab_line_tab_face_functions%entries)) then
            call tab_line_tab_face_functions%append(get_tab_line_tab_face_modified)
            call tab_line_tab_face_functions%append(get_tab_line_tab_face_special)
        end if

        if (.not. allocated(tab_line_new_button_functions%entries)) then
            call tab_line_new_button_functions%append(get_tab_line_tabs_window_buffers)
            call tab_line_new_button_functions%append(get_tab_line_tabs_fixed_window_buffers)
        end if

        if (.not. associated(tab_line_tab_name_function)) tab_line_tab_name_function => get_tab_line_tab_name_buffer
        if (.not. associated(tab_line_tabs_function)) tab_line_tabs_function => get_tab_line_tabs_fixed_window_buffers

        if (.not. associated(tab_line_tabs_buffer_list_function)) then
            tab_line_tabs_buffer_list_function => get_tab_line_tabs_buffer_list
        end if

        if (.not. associated(tab_line_tabs_buffer_group_function)) then
            tab_line_tabs_buffer_group_function => get_tab_line_tabs_buffer_group_by_mode
        end if

        if (.not. associated(tab_line_tabs_buffer_group_sort_function)) then
            tab_line_tabs_buffer_group_sort_function => is_tab_line_buffer_name_less
        end if

        if (.not. associated(tab_line_tabs_buffer_groups_sort_function)) then
            tab_line_tabs_buffer_groups_sort_function => is_string_less_text
        end if

        if (.not. associated(tab_line_tabs_window_buffers_filter_function)) then
            tab_line_tabs_window_buffers_filter_function => keep_all_tab_line_buffers
        end if

        if (.not. associated(tab_line_tab_name_format_function)) then
            tab_line_tab_name_format_function => format_tab_line_tab_name_default
        end if

        ! mouse.el's mouse-buffer-menu-mode-groups.
        if (.not. allocated(tab_line_tabs_buffer_groups)) then
            tab_line_tabs_buffer_groups = [ &
                                          tab_line_buffer_group_type(regular_expression="Info\|Help\|Apropos\|Man", name="Help"), &
                        tab_line_buffer_group_type(regular_expression="\bVM\b\|\bMH\b\|Message\b\|Mail\|Group\|Score\|Summary\|"// &
                                                                     "Article", name="Mail/News"), &
                                          tab_line_buffer_group_type(regular_expression="\<C\>", name="C"), &
                                          tab_line_buffer_group_type(regular_expression="ObjC", name="C"), &
                                          tab_line_buffer_group_type(regular_expression="Text", name="Text"), &
                                          tab_line_buffer_group_type(regular_expression="Outline", name="Text"), &
                                          tab_line_buffer_group_type(regular_expression="\(HT\|SG\|X\|XHT\)ML", name="SGML"), &
                                        tab_line_buffer_group_type(regular_expression="\blog\b\|diff\|\bvc\b\|cvs\|Git\|Annotate", &
                                                                     name="Version Control"), &
                             tab_line_buffer_group_type(regular_expression="Threads\|Memory\|Disassembly\|Breakpoints\|Frames\|"// &
                                                                     "Locals\|Registers\|Inferior I/O\|Debugger", name="GDB"), &
                                          tab_line_buffer_group_type(regular_expression="Lisp", name="Lisp")]
        end if

        call forward_setting(tab_line_exclude, local_if_set=.true.)
        call forward_setting(tab_line_mode_set_explicitly, local_if_set=.true.)
        ! The modes.
        call minor_modes%tab_line_mode%define("tab_line_mode", hook=tab_line_mode_hook, keymap=.true., &
                                              body=tab_line_mode_body)
        tab_line_mode_map = minor_modes%tab_line_mode%map
        call minor_modes%global_tab_line_mode%define("global_tab_line_mode", global=.true., &
                                                     hook=global_tab_line_mode_hook, body=global_tab_line_mode_body)
        ! (add-hook 'tab-line-mode-hook #'tab-line-mode-set-explicitly)
        call tab_line_mode_hook%prepend(set_tab_line_mode_explicitly)
        ! The commands.
        commands%tab_line_mode%execute => tab_line_mode
        commands%global_tab_line_mode%execute => global_tab_line_mode
        commands%switch_to_tab_line_previous_tab%execute => execute_switch_to_tab_line_previous_tab
        commands%switch_to_tab_line_next_tab%execute => execute_switch_to_tab_line_next_tab
        commands%move_tab_line_tab_forward%execute => execute_move_tab_line_tab_forward
        commands%move_tab_line_tab_backward%execute => execute_move_tab_line_tab_backward
        commands%close_tab_line_tab%execute => execute_close_tab_line_tab
        commands%close_tab_line_other_tabs%execute => execute_close_tab_line_other_tabs
        commands%scroll_tab_line_right%execute => execute_scroll_tab_line_right
        commands%scroll_tab_line_left%execute => execute_scroll_tab_line_left
        ! The repeat map.
        if (is_keymap_live(tab_line_switch_repeat_map)) call destroy_keymap(tab_line_switch_repeat_map, st)
        call make_sparse_keymap(tab_line_switch_repeat_map)
        call keymap_set(tab_line_switch_repeat_map, "<left>", "switch_to_tab_line_previous_tab", st)
        call keymap_set(tab_line_switch_repeat_map, "M-<left>", "move_tab_line_tab_backward", st)
        call keymap_set(tab_line_switch_repeat_map, "<right>", "switch_to_tab_line_next_tab", st)
        call keymap_set(tab_line_switch_repeat_map, "M-<right>", "move_tab_line_tab_forward", st)
        call set_repeat_map(tab_line_switch_repeat_map)
    end subroutine initialize_tab_line
end module tab_line_mod
