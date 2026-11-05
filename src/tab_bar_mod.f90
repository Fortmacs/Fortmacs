! tab_bar_mod.f90 -- frame-local tabs with named persistent window configurations.
! Ported from GNU Emacs 31.1 lisp/tab-bar.el (in part: see
! docs/status/tab_bar_mod.md).
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

!> Provides `tab_bar_mode' to enable/disable the tab bar.  Each frame
!> has tabs (the frame parameter `tabs', frame_mod's `tab_type'
!> records): the current tab, and the others, each with the window
!> configuration it restores when selected.  The tab bar is a line at
!> the top of a terminal frame (its `tab_bar_lines'), which redisplay
!> draws from the items `tab_bar_format' makes (display_engine_mod's
!> `get_tab_bar_items').  `tab_bar_history_mode' keeps the window
!> configurations used in each tab.  What is left out is listed in
!> docs/status/tab_bar_mod.md.
module tab_bar_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list, command_type, vtag, xfixnum, V_INTEGER, make_keymap_ref
    use data_mod, only: choice_setting_type
    use string_type_mod, only: string_type, append_string
    use execute_interactively_mod, only: current_prefix_arg, prefix_numeric_value, PREFIX_NONE, PREFIX_NUMBER, this_command
    use keyboard_mod, only: last_command_event, pre_command_hook
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use hooks_mod, only: hook_type
    use character_mod, only: CHAR_MODIFIER_MASK, string_char_and_length
    use faces_mod, only: named_face_type, display_when, face_attrs, face_attributes_type, face_box, &
                         FACE_BOX_RELEASED_BUTTON, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use keymap_mod, only: make_sparse_keymap, is_keymap_live, destroy_keymap, keymap_set, set_repeat_map, key_parse, &
                          define_key, lookup_key, current_global_map, key_modifiers_type
    use buffer_mod, only: buffers, current, current_buffer, resolve_buffer, get_or_make_buffer, is_buffer_live, get_buffer, &
                          frame_seen_buffers, &
                          frame_buried_buffers, exchange_frame_buffers
    use marker_mod, only: make_marker, set_marker, marker_position, marker_buffer
    use editing_functions_mod, only: goto_char
    use time_functions_mod, only: current_time, is_time_less
    use utility_functions_mod, only: count_characters
    use fill_mod, only: string_width
    use multilingual_environment_utilities_mod, only: truncate_string_to_width
    use file_input_output_mod, only: file_exists
    use files_mod, only: find_file_noselect
    use minibuffer_reading_mod, only: minibuf_level, read_from_minibuffer
    use minibuffer_reading_tables_mod, only: completion_list_table
    use minibuffer_mod, only: completing_read, read_buffer, read_file_name, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use frame_mod, only: frame_type, selected_frame, tab_type, tab_window_configuration_type, frame_list, get_frame, &
                         set_tab_bar_lines, delete_frame, default_frame, frame_cols, select_frame_begin, select_frame_end, &
                         is_frame_live, select_frame, ensure_frame_windows
    use window_mod, only: windows, window_leaves, window_point, get_mru_window, select_window, delete_other_windows, &
                          delete_window, split_window, switch_to_buffer, pop_to_buffer, window_display_buffer, &
                          other_buffer, current_window_configuration, set_window_configuration, &
                          destroy_window_configuration, copy_window_configuration, window_state_type, get_window_state, &
                          set_window_state, destroy_window_state, display_action_parameters_type, &
                          window_configuration_change_hook, close_tab_hook, ignore_window_parameters, &
                          window_safe_min_width, same_handle, QR_TAB, WINDOW_SIDE_NONE
    use display_engine_mod, only: message, tab_bar_run_type, get_tab_bar_items
    implicit none(type, external)
    private

    public :: tab_bar_mode, tab_bar_history_mode, init_tab_bar
    public :: select_tab, switch_to_next_tab, switch_to_previous_tab, switch_to_last_tab, switch_to_recent_tab, &
              switch_to_tab, move_tab_to, move_tab, move_tab_backward, make_tab_to, make_tab, duplicate_tab, close_tab, &
              close_tab_by_name, close_other_tabs, undo_close_tab, rename_tab, rename_tab_by_name, move_tab_to_group, &
              change_tab_group, close_group_tabs, switch_to_buffer_other_tab, find_file_other_tab, &
              find_file_read_only_other_tab, toggle_frame_tab_bar, move_window_to_tab, go_back_in_tab_bar_history, &
              go_forward_in_tab_bar_history
    public :: format_tab_bar_history, format_tab_bar_tabs, format_tab_bar_tabs_groups, format_tab_bar_separator, &
              format_tab_bar_add_tab, make_tab_bar_items
    public :: get_tab_name_current, get_tab_name_current_with_count, get_tab_name_truncated, get_tab_name_all
    public :: update_tab_bar_tabs

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Tab bar face for selected tab.  Port of tab-bar.el tab-bar-tab.
    type(named_face_type), target, public :: tab_bar_tab
    !> Tab bar face for non-selected tab.  Port of tab-bar.el
    !> tab-bar-tab-inactive.
    type(named_face_type), target, public :: tab_bar_tab_inactive
    !> Tab bar face for current group tab.  Port of tab-bar.el
    !> tab-bar-tab-group-current.
    type(named_face_type), target, public :: tab_bar_tab_group_current
    !> Tab bar face for inactive group tab.  Port of tab-bar.el
    !> tab-bar-tab-group-inactive.
    type(named_face_type), target, public :: tab_bar_tab_group_inactive
    !> Tab bar face for ungrouped tab when tab groups are used.  Port of
    !> tab-bar.el tab-bar-tab-ungrouped.
    type(named_face_type), target, public :: tab_bar_tab_ungrouped
    !> Tab bar face for highlighting.  Port of tab-bar.el
    !> tab-bar-tab-highlight.
    type(named_face_type), target, public :: tab_bar_tab_highlight

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> Values of `tab_bar_define_keys' besides .true. and .false.
    integer, parameter, public :: TAB_BAR_DEFINE_KEYS_NUMERIC = 2, TAB_BAR_DEFINE_KEYS_TAB = 3

    !> Define specified tab-bar key bindings.
    !> If .true., the default, all key mappings are defined.
    !>
    !> If TAB_BAR_DEFINE_KEYS_NUMERIC, define only numeric select-tab key
    !> mappings, and in conjunction with `tab_bar_select_tab_modifiers',
    !> which see.
    !>
    !> If TAB_BAR_DEFINE_KEYS_TAB, define only TAB and SHIFT-TAB tab-selection
    !> key mappings.
    !>
    !> If .false., do not define any key mappings.  Port of tab-bar.el
    !> tab-bar-define-keys (read when `tab_bar_mode' is turned on; its :set
    !> function waits for Customize).
    type(choice_setting_type), public :: tab_bar_define_keys = choice_setting_type(name="tab_bar_define_keys", value=.true.)

    !> List of modifier keys for selecting tab-bar tabs by their numbers.
    !> Possible modifier keys are `control', `meta', `shift', `hyper', `super' and
    !> `alt' (`tab_bar_select_tab_modifiers = key_modifiers_type(meta=.true.)').
    !> Pressing one of the modifiers in the list and a digit selects the
    !> tab whose number equals the digit (see `select_tab_bar_tab').
    !> The digit 9 selects the last (rightmost) tab (see `last_tab').
    !> The digit 0 selects the most recently visited tab (see `recent_tab').
    !> For easier selection of tabs by their numbers, consider customizing
    !> `tab_bar_tab_hints', which will show tab numbers alongside the tab name.
    !> Port of tab-bar.el tab-bar-select-tab-modifiers (read when
    !> `tab_bar_mode' is turned on; its :set function waits for Customize).
    type(key_modifiers_type), public :: tab_bar_select_tab_modifiers

    !> The setting `tab_bar_show': .true., .false. or a number of tabs
    !> (R6-39: `tab_bar_show%tabs = 1'; negative when no number is set).
    !> Not a port.
    type, extends(choice_setting_type), public :: tab_bar_show_choice_setting_type
        integer :: tabs = -1
    contains
        procedure :: clear_kinds => tab_bar_show_clear_kinds
        procedure :: other_value => tab_bar_show_other_value
        procedure :: kind_hint => tab_bar_show_kind_hint
    end type tab_bar_show_choice_setting_type

    !> Defines when to show the tab bar.
    !> If .true., the default, enable `tab_bar_mode' automatically upon using
    !> the commands that create new window configurations (e.g., `make_tab').
    !> If a number of tabs is set (`tab_bar_show%tabs = N', a non-negative
    !> integer), show the tab bar only if the number of
    !> the tabs exceeds the value of this variable.  In particular,
    !> if the value is 1, hide the tab bar when it has only one tab, and
    !> show it again once more tabs are created.  A value that is a
    !> non-negative integer also makes the tab bar appearance be different
    !> on different frames: the tab bar can be shown on some frames and
    !> hidden on others, depending on how many tab-bar tabs are on that
    !> frame, and whether that number is greater than the numerical value
    !> of this variable.
    !> If 0, always keep the tab bar shown.
    !> If .false., always keep the tab bar hidden.  In this case it's still
    !> possible to use persistent named window configurations by relying on
    !> keyboard commands `make_tab', `close_tab', `next_tab', etc.
    !>
    !> Setting this variable directly does not take effect; please customize
    !> it (see the info node `Easy Customization'), then it will automatically
    !> update the tab bar on all frames according to the new value.
    !>
    !> To enable or disable the tab bar individually on each frame,
    !> you can use the command `toggle_frame_tab_bar'.  Port of tab-bar.el
    !> tab-bar-show (its :set function waits for Customize).
    type(tab_bar_show_choice_setting_type), public :: tab_bar_show = &
                                                      tab_bar_show_choice_setting_type(name="tab_bar_show", value=.true.)

    abstract interface
        !> A buffer to show in a new tab (`tab_bar_new_tab_choice%procedure').
        !> Not a port.
        subroutine tab_bar_buffer_procedure(buffer)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(out) :: buffer
        end subroutine tab_bar_buffer_procedure

        !> A group name for a new tab (`tab_bar_new_tab_group%procedure'),
        !> unallocated for none.  Not a port.
        subroutine tab_bar_group_procedure(group)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: group
        end subroutine tab_bar_group_procedure

        !> A position on the tab bar where to add a new tab
        !> (`tab_bar_new_tab_to%procedure'), counting from 0.  Not a port.
        integer function tab_bar_position_procedure()
            implicit none(type, external)
        end function tab_bar_position_procedure

        !> What to do with TAB, the frame's last tab, when it is closed
        !> (`tab_bar_close_last_tab_choice%procedure').  Not a port.
        subroutine tab_bar_last_tab_procedure(tab, status)
            import :: tab_type, status_type
            implicit none(type, external)
            type(tab_type), intent(in) :: tab
            type(status_type), intent(out) :: status
        end subroutine tab_bar_last_tab_procedure

        !> A tab name (`tab_bar_tab_name_function').  Not a port.
        subroutine tab_name_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
        end subroutine tab_name_procedure
    end interface

    public :: tab_bar_buffer_procedure, tab_bar_group_procedure, tab_bar_position_procedure, tab_bar_last_tab_procedure, &
              tab_name_procedure

    !> Values of `tab_bar_new_tab_choice' besides .true. and .false.
    integer, parameter, public :: TAB_BAR_NEW_TAB_CHOICE_WINDOW = 2, TAB_BAR_NEW_TAB_CHOICE_CLONE = 3

    !> The setting `tab_bar_new_tab_choice': .true., .false., a named
    !> constant, a buffer name or file (R6-39: `%text') or a procedure
    !> (R6-37: `%procedure').  Not a port.
    type, extends(choice_setting_type), public :: tab_bar_new_tab_choice_choice_setting_type
        character(len=:), allocatable :: text
        procedure(tab_bar_buffer_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => tab_bar_new_tab_choice_clear_kinds
        procedure :: other_value => tab_bar_new_tab_choice_other_value
        procedure :: kind_hint => tab_bar_new_tab_choice_kind_hint
    end type tab_bar_new_tab_choice_choice_setting_type

    !> Defines what to show in a new tab.
    !> If .true., start a new tab with the current buffer, i.e. the buffer
    !> that was current before calling the command that adds a new tab
    !> (this is the same what `make_frame' does by default).
    !> If TAB_BAR_NEW_TAB_CHOICE_WINDOW, then keep the selected
    !> window as a single window on the new tab, and keep all its
    !> window parameters except `window_atom' and `window_side'.
    !> If its name is set (`tab_bar_new_tab_choice%text = "*scratch*"'),
    !> use it as a buffer name to switch to
    !> if such buffer exists, or switch to a buffer visiting the file or
    !> directory that the string specifies.  If its procedure is set
    !> (`tab_bar_new_tab_choice%procedure => f'),
    !> call it and switch to the buffer that it gives.
    !> If TAB_BAR_NEW_TAB_CHOICE_CLONE, duplicate the contents of the tab
    !> that was active before calling the command that adds a new tab.
    !> Port of tab-bar.el tab-bar-new-tab-choice.
    type(tab_bar_new_tab_choice_choice_setting_type), public :: tab_bar_new_tab_choice = &
                                             tab_bar_new_tab_choice_choice_setting_type(name="tab_bar_new_tab_choice", value=.true.)

    !> The setting `tab_bar_new_tab_group': .true., .false., a group name
    !> (R6-39: `%text') or a procedure (R6-37: `%procedure').  Not a port.
    type, extends(choice_setting_type), public :: tab_bar_new_tab_group_choice_setting_type
        character(len=:), allocatable :: text
        procedure(tab_bar_group_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => tab_bar_new_tab_group_clear_kinds
        procedure :: other_value => tab_bar_new_tab_group_other_value
        procedure :: kind_hint => tab_bar_new_tab_group_kind_hint
    end type tab_bar_new_tab_group_choice_setting_type

    !> Defines what group to assign to a new tab.
    !> If .false., don't set a default group automatically.
    !> If .true., inherit the group name from the previous tab.
    !> If its name is set (`tab_bar_new_tab_group%text = "..."'), use it as
    !> the group name of a new tab.
    !> If its procedure is set (`tab_bar_new_tab_group%procedure => f'),
    !> call it to get the group name.  Port of tab-bar.el
    !> tab-bar-new-tab-group.
    type(tab_bar_new_tab_group_choice_setting_type), public :: tab_bar_new_tab_group = &
                                               tab_bar_new_tab_group_choice_setting_type(name="tab_bar_new_tab_group", value=.true.)

    !> If true, show the "New tab" button in the tab bar.
    !> When this is .false., you can create new tabs with \[make_tab].
    !> Port of tab-bar.el tab-bar-new-button-show (obsolete since 28.1 in
    !> favor of `tab_bar_format').
    logical, public :: tab_bar_new_button_show = .true.

    !> Button for creating a new tab.  Port of tab-bar.el
    !> tab-bar-new-button.
    character(len=:), allocatable, public :: tab_bar_new_button

    !> Values of `tab_bar_close_button_show' besides .true. and .false.
    integer, parameter, public :: TAB_BAR_CLOSE_BUTTON_SHOW_SELECTED = 2, TAB_BAR_CLOSE_BUTTON_SHOW_NON_SELECTED = 3

    !> Defines where to show the close tab button.
    !> If .true., show the close tab button on all tabs.
    !> If TAB_BAR_CLOSE_BUTTON_SHOW_SELECTED, show it only on the selected tab.
    !> If TAB_BAR_CLOSE_BUTTON_SHOW_NON_SELECTED, show it only on non-selected tab.
    !> If .false., don't show it at all.  Port of tab-bar.el
    !> tab-bar-close-button-show.
    type(choice_setting_type), public :: tab_bar_close_button_show = &
                                         choice_setting_type(name="tab_bar_close_button_show", value=.true.)

    !> Button for closing the clicked tab.  Port of tab-bar.el
    !> tab-bar-close-button.
    character(len=:), allocatable, public :: tab_bar_close_button

    !> Button for going back in tab history.  Port of tab-bar.el
    !> tab-bar-back-button.
    character(len=:), allocatable, public :: tab_bar_back_button

    !> Button for going forward in tab history.  Port of tab-bar.el
    !> tab-bar-forward-button.
    character(len=:), allocatable, public :: tab_bar_forward_button

    !> Show absolute numbers on tabs in the tab bar before the tab name.
    !> This helps to select the tab by its number using `select_tab'
    !> and `tab_bar_select_tab_modifiers'.  Port of tab-bar.el
    !> tab-bar-tab-hints.
    logical, public :: tab_bar_tab_hints = .false.

    !> String that delimits tabs (unallocated for the default: a space on a
    !> window system, "|" on a terminal).  Port of tab-bar.el
    !> tab-bar-separator.
    character(len=:), allocatable, public :: tab_bar_separator

    !> Function to get a tab name.
    !> Function gets no arguments.
    !> The choice is between displaying only the name of the current buffer
    !> in the tab name (default), or displaying the names of all buffers
    !> from all windows in the window configuration: `get_tab_name_current',
    !> `get_tab_name_current_with_count', `get_tab_name_truncated' and
    !> `get_tab_name_all'.  Port of tab-bar.el tab-bar-tab-name-function.
    procedure(tab_name_procedure), pointer, public :: tab_bar_tab_name_function => get_tab_name_current

    !> Maximum length of the tab name from the current buffer.
    !> Effective when `tab_bar_tab_name_function' is customized
    !> to `get_tab_name_truncated'.  Port of tab-bar.el
    !> tab-bar-tab-name-truncated-max.
    integer, public :: tab_bar_tab_name_truncated_max = 20

    !> An item of the tab bar (a menu item of tab-bar.el's
    !> tab-bar-make-keymap-1): its key (`sep-1', `current-tab', `tab-2',
    !> `group-3', `add-tab', ...) and its caption.  Not a port.
    type, public :: tab_bar_item_type
        character(len=32) :: key = ""
        type(tab_bar_run_type), allocatable :: runs(:)
    end type tab_bar_item_type

    abstract interface
        !> An element of `tab_bar_format': the items it adds to the tab
        !> bar (unallocated for none).  Not a port.
        subroutine tab_bar_format_procedure(items)
            import :: tab_bar_item_type
            implicit none(type, external)
            type(tab_bar_item_type), allocatable, intent(out) :: items(:)
        end subroutine tab_bar_format_procedure
    end interface

    public :: tab_bar_format_procedure

    !> An element of `tab_bar_format'.  Not a port.
    type :: tab_bar_format_entry_type
        procedure(tab_bar_format_procedure), pointer, nopass :: item => null()
    end type tab_bar_format_entry_type

    !> The procedures of `tab_bar_format', in order.  Not a port.
    type, public :: tab_bar_format_type
        type(tab_bar_format_entry_type), allocatable :: entries(:)
    contains
        !> Add a procedure at the end.
        procedure :: append => tab_bar_format_append
        !> Add a procedure at the start.
        procedure :: prepend => tab_bar_format_prepend
        !> Remove a procedure.
        procedure :: remove => tab_bar_format_remove
    end type tab_bar_format_type

    !> Template for displaying tab bar items.
    !> Every item in the list is a procedure that gives tab bar items, or
    !> none.  Adding a procedure to the list (`call
    !> tab_bar_format%append(format_tab_bar_tabs_groups)') causes the tab bar
    !> to show its items.  Replacing `format_tab_bar_tabs' with
    !> `format_tab_bar_tabs_groups' will group tabs on the tab bar.  The
    !> default is `format_tab_bar_history', `format_tab_bar_tabs',
    !> `format_tab_bar_separator' and `format_tab_bar_add_tab'.  Port of
    !> tab-bar.el tab-bar-format.
    type(tab_bar_format_type), public :: tab_bar_format

    !> Show tabs even if they are in inactive groups.  Port of tab-bar.el
    !> tab-bar-show-inactive-group-tabs.
    logical, public :: tab_bar_show_inactive_group_tabs = .false.

    !> Automatically resize width of tabs on tab bar to fill available tab-bar space.
    !> When true, the widths of the tabs on the tab bar are
    !> automatically resized so that their width is evenly distributed
    !> across the tab bar.  This keeps the widths of the tabs
    !> independent of the length of the buffer names shown on each tab;
    !> the tab widths change only when tabs are added or deleted, or
    !> when the frame's dimensions change.  This also avoids as much as
    !> possible wrapping a long tab bar to a second tab-bar line.
    !>
    !> The automatic resizing of tabs takes place as long as tabs are no
    !> wider than 20 columns and at least 2 columns wide on a terminal.
    !>
    !> When this variable is .false., the width of each tab is determined
    !> by the length of the tab's name.  Port of tab-bar.el
    !> tab-bar-auto-width.
    logical, public :: tab_bar_auto_width = .true.

    !> The setting `tab_bar_new_tab_to': a named constant or a procedure
    !> (R6-37: `%procedure').  Not a port.
    type, extends(choice_setting_type), public :: tab_bar_new_tab_to_choice_setting_type
        procedure(tab_bar_position_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => tab_bar_new_tab_to_clear_kinds
        procedure :: other_value => tab_bar_new_tab_to_other_value
        procedure :: kind_hint => tab_bar_new_tab_to_kind_hint
    end type tab_bar_new_tab_to_choice_setting_type

    !> Values of `tab_bar_new_tab_to'.
    integer, parameter, public :: TAB_BAR_NEW_TAB_TO_LEFTMOST = 2, TAB_BAR_NEW_TAB_TO_LEFT = 3, &
                                  TAB_BAR_NEW_TAB_TO_RIGHT = 4, TAB_BAR_NEW_TAB_TO_RIGHTMOST = 5

    !> Where to create a new tab.
    !> If TAB_BAR_NEW_TAB_TO_LEFTMOST, create as the first tab.
    !> If TAB_BAR_NEW_TAB_TO_LEFT, create to the left of the current tab.
    !> If TAB_BAR_NEW_TAB_TO_RIGHT, create to the right of the current tab.
    !> If TAB_BAR_NEW_TAB_TO_RIGHTMOST, create as the last tab.
    !> If its procedure is set (`tab_bar_new_tab_to%procedure => f'), it
    !> should return a number as a position on the tab bar specifying where
    !> to add a new tab.  Port of tab-bar.el tab-bar-new-tab-to.
    type(tab_bar_new_tab_to_choice_setting_type), public :: tab_bar_new_tab_to = &
                                tab_bar_new_tab_to_choice_setting_type(name="tab_bar_new_tab_to", choice=TAB_BAR_NEW_TAB_TO_RIGHT, &
                                                                                            true_valid=.false., false_valid=.false.)

    !> Values of `tab_bar_close_tab_select'.
    integer, parameter, public :: TAB_BAR_CLOSE_TAB_SELECT_LEFT = 2, TAB_BAR_CLOSE_TAB_SELECT_RIGHT = 3, &
                                  TAB_BAR_CLOSE_TAB_SELECT_RECENT = 4

    !> Which tab to make current after closing the specified tab.
    !> If TAB_BAR_CLOSE_TAB_SELECT_LEFT, select the adjacent left tab.
    !> If TAB_BAR_CLOSE_TAB_SELECT_RIGHT, select the adjacent right tab.
    !> If TAB_BAR_CLOSE_TAB_SELECT_RECENT, select the most recently visited
    !> tab.  Port of tab-bar.el tab-bar-close-tab-select.
    type(choice_setting_type), public :: tab_bar_close_tab_select = &
                                     choice_setting_type(name="tab_bar_close_tab_select", true_valid=.false., false_valid=.false., &
                                                             choice=TAB_BAR_CLOSE_TAB_SELECT_RECENT)

    !> The setting `tab_bar_close_last_tab_choice': .false., a named
    !> constant or a procedure (R6-37: `%procedure').  Not a port.
    type, extends(choice_setting_type), public :: tab_bar_close_last_tab_choice_choice_setting_type
        procedure(tab_bar_last_tab_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => tab_bar_close_last_tab_choice_clear_kinds
        procedure :: other_value => tab_bar_close_last_tab_choice_other_value
        procedure :: kind_hint => tab_bar_close_last_tab_choice_kind_hint
    end type tab_bar_close_last_tab_choice_choice_setting_type

    !> Values of `tab_bar_close_last_tab_choice' besides .false.
    integer, parameter, public :: TAB_BAR_CLOSE_LAST_TAB_CHOICE_DELETE_FRAME = 2, &
                                  TAB_BAR_CLOSE_LAST_TAB_CHOICE_TAB_BAR_MODE_DISABLE = 3

    !> What to do when the last tab is closed.
    !> If .false., do nothing and show a message, like closing the last window or frame.
    !> If TAB_BAR_CLOSE_LAST_TAB_CHOICE_DELETE_FRAME, delete the containing
    !> frame, as a web browser would do.
    !> If TAB_BAR_CLOSE_LAST_TAB_CHOICE_TAB_BAR_MODE_DISABLE, disable
    !> `tab_bar_mode' so that tabs no longer show in the frame.
    !> If its procedure is set (`tab_bar_close_last_tab_choice%procedure =>
    !> f'), call that procedure with the tab to be closed as an argument.
    !> Port of tab-bar.el tab-bar-close-last-tab-choice.
    type(tab_bar_close_last_tab_choice_choice_setting_type), public :: tab_bar_close_last_tab_choice = &
                         tab_bar_close_last_tab_choice_choice_setting_type(name="tab_bar_close_last_tab_choice", true_valid=.false.)

    !> The number of history elements to keep.  Port of tab-bar.el
    !> tab-bar-history-limit.
    integer, public :: tab_bar_history_limit = 10

    !> A closed tab, to be able to undo its closing: its frame, its index
    !> (from 0) and the tab.  Not a port.
    type, public :: closed_tab_type
        type(handle_type) :: frame
        integer :: index = 0
        type(tab_type) :: tab
    end type closed_tab_type

    !> A list of closed tabs to be able to undo their closing, the most
    !> recently closed first.  Port of tab-bar.el tab-bar-closed-tabs.
    type(closed_tab_type), allocatable, public :: tab_bar_closed_tabs(:)

    ! ------------------------------------------------------------------
    ! Modes and keymaps
    ! ------------------------------------------------------------------

    !> Hook run after entering or leaving `tab_bar_mode'.  Port of
    !> tab-bar.el tab-bar-mode-hook.
    type(hook_type), target, public :: tab_bar_mode_hook = hook_type(name="tab_bar_mode_hook")

    !> Hook run after entering or leaving `tab_bar_history_mode'.  Port of
    !> tab-bar.el tab-bar-history-mode-hook.
    type(hook_type), target, public :: tab_bar_history_mode_hook = hook_type(name="tab_bar_history_mode_hook")

    !> Tab Bar mode map.  Port of tab-bar.el tab-bar-mode-map.
    type(handle_type), public :: tab_bar_mode_map

    !> Keymap of `tab_bar_history_mode'.  Port of tab-bar.el
    !> tab-bar-history-mode-map.
    type(handle_type), public :: tab_bar_history_mode_map

    !> Keymap to repeat tab-bar-history key sequences.
    !> Used in `repeat_mode'.  Port of tab-bar.el tab-bar-history-repeat-map.
    type(handle_type), public :: tab_bar_history_repeat_map

    !> Keymap to repeat tab switch commands `next_tab' and
    !> `previous_tab'.  Used in `repeat_mode'.  Port of
    !> tab-bar.el tab-bar-switch-repeat-map.
    type(handle_type), public :: tab_bar_switch_repeat_map

    !> Keymap to repeat tab move commands `move_tab' and
    !> `move_tab_bar_tab_backward'.  Used in `repeat_mode'.  Port of
    !> tab-bar.el tab-bar-move-repeat-map.
    type(handle_type), public :: tab_bar_move_repeat_map

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    !> When true, omit window-configuration changes from the current
    !> command.  Port of tab-bar.el tab-bar-history-omit.
    logical :: tab_bar_history_omit = .false.

    !> Window configuration before the current command (none when
    !> `has_tab_bar_history_old' is false).  Port of tab-bar.el
    !> tab-bar-history-old.
    type(tab_window_configuration_type) :: tab_bar_history_old
    logical :: has_tab_bar_history_old = .false.

    !> Command set to `this_command' by `pre_command_hook', and the command
    !> handled by `window_configuration_change_hook'.  Port of tab-bar.el
    !> tab-bar-history-pre-command and tab-bar-history-done-command.
    type(command_type), pointer :: tab_bar_history_pre_command => null()
    type(command_type), pointer :: tab_bar_history_done_command => null()

    !> No messages while true (an `inhibit-message' binding).  Not a port.
    logical :: inhibit_tab_messages = .false.

    !> The faces of a formatted tab name: the tab faces, or the group
    !> faces (tab-bar.el's tab-bar-tab-face-default and
    !> tab-bar-tab-group-face-default as `tab-bar-tab-face-function').
    integer, parameter :: TAB_FACES = 1, GROUP_FACES = 2

    !> One character of a tab bar item's caption while its width is made
    !> (tab-bar.el's tab-bar-auto-width works on the characters and their
    !> text properties): its bytes, faces and `close-tab' property.
    type :: caption_char_type
        character(len=:), allocatable :: text
        character(len=40) :: faces(3) = ""
        logical :: close_tab = .false.
    end type caption_char_type

contains

    ! ------------------------------------------------------------------
    ! The kinds of the settings
    ! ------------------------------------------------------------------

    !> Forget the number of tabs of `tab_bar_show'.  Not a port.
    subroutine tab_bar_show_clear_kinds(self)
        class(tab_bar_show_choice_setting_type), intent(inout) :: self

        self%tabs = -1
    end subroutine tab_bar_show_clear_kinds

    !> The number of tabs of `tab_bar_show' as help shows it (`%tabs = N');
    !> "" when none is set.  Not a port.
    function tab_bar_show_other_value(self) result(text)
        class(tab_bar_show_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=32) :: component_text

        text = ""

        if (self%tabs >= 0) then
            write (component_text, '("%tabs = ", i0)') self%tabs
            text = trim(component_text)
        end if
    end function tab_bar_show_other_value

    !> The assignment that sets the number `tab_bar_show' takes, for the
    !> error of a rejected value.  Not a port.
    function tab_bar_show_kind_hint(self, number) result(text)
        class(tab_bar_show_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of tabs is `"//trim(self%name)//"%tabs = "//number//"'"
    end function tab_bar_show_kind_hint

    !> Forget the name and procedure of `tab_bar_new_tab_choice'.
    !> Not a port.
    subroutine tab_bar_new_tab_choice_clear_kinds(self)
        class(tab_bar_new_tab_choice_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine tab_bar_new_tab_choice_clear_kinds

    !> The name or procedure of `tab_bar_new_tab_choice' as help shows it;
    !> "" when neither is set.  Not a port.
    function tab_bar_new_tab_choice_other_value(self) result(text)
        class(tab_bar_new_tab_choice_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%text)) text = "%text = """//self%text//""""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function tab_bar_new_tab_choice_other_value

    !> The assignments that set the name or procedure
    !> `tab_bar_new_tab_choice' takes, for the error of a rejected value.
    !> Not a port.
    function tab_bar_new_tab_choice_kind_hint(self, number) result(text)
        class(tab_bar_new_tab_choice_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a buffer or file name is `"//trim(self%name)//"%text = ""...""' and a procedure `"// &
               trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function tab_bar_new_tab_choice_kind_hint

    !> Forget the name and procedure of `tab_bar_new_tab_group'.
    !> Not a port.
    subroutine tab_bar_new_tab_group_clear_kinds(self)
        class(tab_bar_new_tab_group_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine tab_bar_new_tab_group_clear_kinds

    !> The name or procedure of `tab_bar_new_tab_group' as help shows it;
    !> "" when neither is set.  Not a port.
    function tab_bar_new_tab_group_other_value(self) result(text)
        class(tab_bar_new_tab_group_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%text)) text = "%text = """//self%text//""""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function tab_bar_new_tab_group_other_value

    !> The assignments that set the name or procedure
    !> `tab_bar_new_tab_group' takes, for the error of a rejected value.
    !> Not a port.
    function tab_bar_new_tab_group_kind_hint(self, number) result(text)
        class(tab_bar_new_tab_group_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a group name is `"//trim(self%name)//"%text = ""...""' and a procedure `"// &
               trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function tab_bar_new_tab_group_kind_hint

    !> Forget the procedure of `tab_bar_new_tab_to'.  Not a port.
    subroutine tab_bar_new_tab_to_clear_kinds(self)
        class(tab_bar_new_tab_to_choice_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine tab_bar_new_tab_to_clear_kinds

    !> The procedure of `tab_bar_new_tab_to' as help shows it; "" when none
    !> is set.  Not a port.
    function tab_bar_new_tab_to_other_value(self) result(text)
        class(tab_bar_new_tab_to_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function tab_bar_new_tab_to_other_value

    !> The assignment that sets the procedure `tab_bar_new_tab_to' takes,
    !> for the error of a rejected value.  Not a port.
    function tab_bar_new_tab_to_kind_hint(self, number) result(text)
        class(tab_bar_new_tab_to_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function tab_bar_new_tab_to_kind_hint

    !> Forget the procedure of `tab_bar_close_last_tab_choice'.
    !> Not a port.
    subroutine tab_bar_close_last_tab_choice_clear_kinds(self)
        class(tab_bar_close_last_tab_choice_choice_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine tab_bar_close_last_tab_choice_clear_kinds

    !> The procedure of `tab_bar_close_last_tab_choice' as help shows it;
    !> "" when none is set.  Not a port.
    function tab_bar_close_last_tab_choice_other_value(self) result(text)
        class(tab_bar_close_last_tab_choice_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function tab_bar_close_last_tab_choice_other_value

    !> The assignment that sets the procedure
    !> `tab_bar_close_last_tab_choice' takes, for the error of a rejected
    !> value.  Not a port.
    function tab_bar_close_last_tab_choice_kind_hint(self, number) result(text)
        class(tab_bar_close_last_tab_choice_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function tab_bar_close_last_tab_choice_kind_hint

    !> Add ITEM at the end of SELF.  Not a port.
    subroutine tab_bar_format_append(self, item)
        class(tab_bar_format_type), intent(inout) :: self
        procedure(tab_bar_format_procedure) :: item
        type(tab_bar_format_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%item => item
        self%entries = [self%entries, entry]
    end subroutine tab_bar_format_append

    !> Add ITEM at the start of SELF.  Not a port.
    subroutine tab_bar_format_prepend(self, item)
        class(tab_bar_format_type), intent(inout) :: self
        procedure(tab_bar_format_procedure) :: item
        type(tab_bar_format_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%item => item
        self%entries = [entry, self%entries]
    end subroutine tab_bar_format_prepend

    !> Remove ITEM from SELF.  Not a port.
    subroutine tab_bar_format_remove(self, item)
        class(tab_bar_format_type), intent(inout) :: self
        procedure(tab_bar_format_procedure) :: item
        type(tab_bar_format_entry_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(self%entries)) return
        allocate (kept(0))

        do k = 1, size(self%entries)
            if (.not. associated(self%entries(k)%item, item)) kept = [kept, self%entries(k)]
        end do

        call move_alloc(kept, self%entries)
    end subroutine tab_bar_format_remove

    ! ------------------------------------------------------------------
    ! Tab names
    ! ------------------------------------------------------------------

    !> Generate tab name from the buffer of the selected window.  Port of
    !> tab-bar.el:tab-bar-tab-name-current (the window the minibuffer was
    !> entered from while it is selected).
    subroutine get_tab_name_current(name)
        character(len=:), allocatable, intent(out) :: name
        integer(int64) :: bi
        type(status_type) :: status

        name = ""
        call ensure_frame_windows()
        call resolve_buffer(windows(selected_frame%selected_window%index)%buffer, bi, status)
        if (status%ok()) name = buffers(bi)%name
    end subroutine get_tab_name_current

    !> Generate tab name from the buffer of the selected window.
    !> Also add the number of windows in the window configuration.
    !> Port of tab-bar.el:tab-bar-tab-name-current-with-count.
    subroutine get_tab_name_current_with_count(name)
        character(len=:), allocatable, intent(out) :: name
        type(handle_type), allocatable :: leaves(:)
        character(len=24) :: count_text

        call get_tab_name_current(name)
        call window_leaves(selected_frame%root_window, leaves)

        if (size(leaves) > 1) then
            write (count_text, '(" (", i0, ")")') size(leaves)
            name = name//trim(count_text)
        end if
    end subroutine get_tab_name_current_with_count

    !> Generate tab name from buffers of all windows.  Port of
    !> tab-bar.el:tab-bar-tab-name-all.
    subroutine get_tab_name_all(name)
        character(len=:), allocatable, intent(out) :: name
        type(handle_type), allocatable :: leaves(:), seen(:)
        integer(int64) :: bi
        type(status_type) :: status
        integer :: k

        name = ""
        call ensure_frame_windows()
        call window_leaves(selected_frame%root_window, leaves)
        allocate (seen(0))

        do k = 1, size(leaves)
            if (any(same_handle(seen, windows(leaves(k)%index)%buffer))) cycle
            seen = [seen, windows(leaves(k)%index)%buffer]
            call resolve_buffer(windows(leaves(k)%index)%buffer, bi, status)
            if (.not. status%ok()) cycle

            if (size(seen) > 1) then
                name = name//", "//buffers(bi)%name
            else
                name = buffers(bi)%name
            end if
        end do
    end subroutine get_tab_name_all

    !> Generate tab name from the buffer of the selected window.
    !> Truncate it to the length specified by `tab_bar_tab_name_truncated_max'.
    !> Append an ellipsis (`truncate_string_ellipsis') in this case.
    !> Port of tab-bar.el:tab-bar-tab-name-truncated (with
    !> `tab-bar-tab-name-ellipsis' t).
    subroutine get_tab_name_truncated(name)
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable :: tab_name

        call get_tab_name_current(tab_name)

        if (count_characters(tab_name) < tab_bar_tab_name_truncated_max) then
            name = tab_name
        else
            name = truncate_string_to_width(tab_name, tab_bar_tab_name_truncated_max, default_ellipsis=.true.)
        end if
    end subroutine get_tab_name_truncated

    ! ------------------------------------------------------------------
    ! Tabs
    ! ------------------------------------------------------------------

    !> The index of the selected frame's current tab, 0 for none.  Port of
    !> tab-bar.el:tab-bar--current-tab-index (from 1).
    integer function current_tab_index() result(k)
        k = 0
        if (.not. allocated(selected_frame%parameters%tabs)) return
        if (size(selected_frame%parameters%tabs) == 0) return
        k = findloc(selected_frame%parameters%tabs%current, .true., dim=1)
    end function current_tab_index

    !> The number of the selected frame's tabs.  Not a port.
    integer function tab_count() result(n)
        n = 0
        if (allocated(selected_frame%parameters%tabs)) n = size(selected_frame%parameters%tabs)
    end function tab_count

    !> The index of the selected frame's tab named NAME, 0 for none.
    !> Port of tab-bar.el:tab-bar--tab-index-by-name (from 1).
    integer function tab_index_by_name(name) result(k)
        character(len=*), intent(in) :: name
        integer :: j

        k = 0

        do j = 1, tab_count()
            if (selected_frame%parameters%tabs(j)%name == name .and. &
                len(selected_frame%parameters%tabs(j)%name) == len(name)) then
                k = j
                return
            end if
        end do
    end function tab_index_by_name

    !> The indices of the selected frame's tabs other than the current one,
    !> the most recently visited first.  Port of
    !> tab-bar.el:tab-bar--tabs-recent.
    function tabs_recent() result(order)
        integer, allocatable :: order(:)
        integer :: k, j, i

        allocate (order(0))

        do k = 1, tab_count()
            if (selected_frame%parameters%tabs(k)%current) cycle
            ! A stable insertion: after the tabs as recent or more.
            j = size(order) + 1

            do i = 1, size(order)
                if (is_time_less(selected_frame%parameters%tabs(order(i))%time, &
                                 selected_frame%parameters%tabs(k)%time)) then
                    j = i
                    exit
                end if
            end do

            order = [order(1:j - 1), k, order(j:)]
        end do
    end function tabs_recent

    !> The index of the NTH most recently visited tab, 0 for none.
    !> Port of tab-bar.el:tab-bar--tab-index-recent (from 1).
    integer function tab_index_recent(nth) result(k)
        integer, intent(in) :: nth
        integer, allocatable :: order(:)

        k = 0
        order = tabs_recent()
        if (nth >= 1 .and. nth <= size(order)) k = order(nth)
    end function tab_index_recent

    !> Make the current tab data structure, with TEMPLATE as template: the
    !> tab is created using data from TEMPLATE.  This is necessary when
    !> switching tabs, otherwise the destination tab inherits the current
    !> tab's `explicit_name' parameter.  Port of
    !> tab-bar.el:tab-bar--current-tab-make.
    subroutine make_current_tab(tab, template)
        type(tab_type), intent(out) :: tab
        type(tab_type), intent(in), optional :: template
        character(len=:), allocatable :: group

        tab%current = .true.
        tab%explicit_name = .false.
        if (present(template)) tab%explicit_name = template%explicit_name

        if (tab%explicit_name) then
            tab%name = template%name
        else
            call tab_bar_tab_name_function(tab%name)
        end if

        if (present(template)) then
            if (allocated(template%group)) tab%group = template%group
        else if (allocated(tab_bar_new_tab_group%text)) then
            tab%group = tab_bar_new_tab_group%text
        else if (associated(tab_bar_new_tab_group%procedure)) then
            call tab_bar_new_tab_group%procedure(group)
            if (allocated(group)) tab%group = group
        end if
    end subroutine make_current_tab

    !> Ensure the selected frame's tabs exist: make the first one if there
    !> is none, else update the current tab name unless the user gave it.
    !> Port of tab-bar.el:tab-bar-tabs (FRAME the selected frame).
    subroutine update_tab_bar_tabs()
        integer :: k

        call ensure_frame_windows()

        if (tab_count() > 0) then
            k = current_tab_index()

            if (k > 0) then
                if (.not. selected_frame%parameters%tabs(k)%explicit_name) then
                    call tab_bar_tab_name_function(selected_frame%parameters%tabs(k)%name)
                end if
            end if

            return
        end if

        ! Create default tabs.
        if (allocated(selected_frame%parameters%tabs)) deallocate (selected_frame%parameters%tabs)
        allocate (selected_frame%parameters%tabs(1))
        call make_current_tab(selected_frame%parameters%tabs(1))
    end subroutine update_tab_bar_tabs

    !> Make COPY a copy of window configuration FROM with markers of its
    !> own.  Not a port (upstream shares the Lisp objects).
    subroutine copy_tab_window_configuration(from, copy)
        type(tab_window_configuration_type), intent(in) :: from
        type(tab_window_configuration_type), intent(out) :: copy
        type(handle_type) :: b
        integer(int64) :: pos
        type(status_type) :: status

        if (allocated(from%window_configuration%saved)) then
            call copy_window_configuration(from%window_configuration, copy%window_configuration)
        end if

        copy%point = null_handle()
        if (from%point%index == 0) return
        call marker_buffer(from%point, b, status)
        if (.not. status%ok() .or. .not. is_buffer_live(b)) return
        call marker_position(from%point, pos, status)
        if (.not. status%ok()) return
        call make_marker(copy%point)
        call set_marker(copy%point, pos, status, b)
    end subroutine copy_tab_window_configuration

    !> Copies of the window configurations FROM, in COPIES.  Not a port.
    subroutine copy_history(from, copies)
        type(tab_window_configuration_type), intent(in) :: from(:)
        type(tab_window_configuration_type), allocatable, intent(out) :: copies(:)
        integer :: k

        allocate (copies(size(from)))

        do k = 1, size(from)
            call copy_tab_window_configuration(from(k), copies(k))
        end do
    end subroutine copy_history

    !> Free the window configurations HISTORY.  Not a port.
    subroutine destroy_history(history)
        type(tab_window_configuration_type), allocatable, intent(inout) :: history(:)
        integer :: k

        if (.not. allocated(history)) return

        do k = 1, size(history)
            call history(k)%destroy()
        end do

        deallocate (history)
    end subroutine destroy_history

    !> The selected frame's window configuration and the point of its
    !> selected window, in CONFIG.  Not a port.
    subroutine current_tab_window_configuration(config)
        type(tab_window_configuration_type), intent(out) :: config
        type(handle_type) :: b
        integer(int64) :: bi
        type(status_type) :: status

        call current_window_configuration(config%window_configuration)
        b = windows(selected_frame%selected_window%index)%buffer
        call resolve_buffer(b, bi, status)
        if (.not. status%ok()) return
        call make_marker(config%point)
        call set_marker(config%point, buffers(bi)%pt, status, b)
    end subroutine current_tab_window_configuration

    !> Make a new tab data structure that can be added to the selected
    !> frame's tabs: the current tab with its window configuration.
    !> Port of tab-bar.el:tab-bar--tab (`tab_bar_select_restore_context' is
    !> Deferred).
    subroutine make_tab_from_current(tab)
        type(tab_type), intent(out) :: tab
        type(handle_type), allocatable :: list(:)
        integer :: k, j

        call update_tab_bar_tabs()
        k = current_tab_index()
        tab%current = .false.

        if (k > 0) then
            tab%explicit_name = selected_frame%parameters%tabs(k)%explicit_name
            if (allocated(selected_frame%parameters%tabs(k)%group)) tab%group = selected_frame%parameters%tabs(k)%group
        end if

        if (tab%explicit_name) then
            tab%name = selected_frame%parameters%tabs(k)%name
        else
            call tab_bar_tab_name_function(tab%name)
        end if

        tab%time = current_time()
        call current_tab_window_configuration(tab%window_configuration)
        tab%has_window_configuration = .true.
        list = frame_seen_buffers()
        allocate (tab%buffers(0))

        do j = 1, size(list)
            if (is_buffer_live(list(j))) tab%buffers = [tab%buffers, list(j)]
        end do

        list = frame_buried_buffers()
        allocate (tab%buried_buffers(0))

        do j = 1, size(list)
            if (is_buffer_live(list(j))) tab%buried_buffers = [tab%buried_buffers, list(j)]
        end do

        if (minor_modes%tab_bar_history_mode%enabled()) then
            tab%has_history = .true.

            if (allocated(selected_frame%tab_history_back)) then
                call copy_history(selected_frame%tab_history_back, tab%history_back)
            else
                allocate (tab%history_back(0))
            end if

            if (allocated(selected_frame%tab_history_forward)) then
                call copy_history(selected_frame%tab_history_forward, tab%history_forward)
            else
                allocate (tab%history_forward(0))
            end if
        end if
    end subroutine make_tab_from_current

    !> Insert TAB at index K (from 1) of the selected frame's tabs.
    !> Not a port (cl-pushnew on an nthcdr).
    subroutine insert_tab(tab, k)
        type(tab_type), intent(in) :: tab
        integer, intent(in) :: k
        type(tab_type), allocatable :: grown(:)
        integer :: n

        n = tab_count()
        allocate (grown(n + 1))
        if (k > 1) grown(1:k - 1) = selected_frame%parameters%tabs(1:k - 1)
        grown(k) = tab
        if (k <= n) grown(k + 1:) = selected_frame%parameters%tabs(k:)
        call move_alloc(grown, selected_frame%parameters%tabs)
    end subroutine insert_tab

    !> Take tab K (from 1) out of the selected frame's tabs, without
    !> freeing it (delq).  Not a port.
    subroutine remove_tab(k)
        integer, intent(in) :: k
        type(tab_type), allocatable :: kept(:)
        integer :: n

        n = tab_count()
        allocate (kept(n - 1))
        if (k > 1) kept(1:k - 1) = selected_frame%parameters%tabs(1:k - 1)
        if (k < n) kept(k:) = selected_frame%parameters%tabs(k + 1:)
        call move_alloc(kept, selected_frame%parameters%tabs)
    end subroutine remove_tab

    !> Show TEXT in the echo area unless `inhibit_tab_messages'.
    !> Not a port (`message' under `inhibit-message').
    subroutine tab_message(text)
        character(len=*), intent(in) :: text

        if (.not. inhibit_tab_messages) call message(text)
    end subroutine tab_message

    ! ------------------------------------------------------------------
    ! The tab bar lines
    ! ------------------------------------------------------------------

    !> Determine and return the value of `tab-bar-lines' for FRAME.
    !> Return 0 if `tab_bar_mode' is not enabled.  Otherwise return
    !> either 1 or 0 depending on the value of the customizable variable
    !> `tab_bar_show', which see.  Port of
    !> tab-bar.el:tab-bar--tab-bar-lines-for-frame.
    integer function tab_bar_lines_for_frame(frame) result(lines)
        type(handle_type), intent(in) :: frame
        type(handle_type) :: saved

        lines = 0
        if (.not. minor_modes%tab_bar_mode%enabled()) return
        if (tab_bar_show%is_off()) return

        if (tab_bar_show%tabs >= 0) then
            call select_frame_begin(frame, saved)
            call update_tab_bar_tabs()
            if (tab_count() > tab_bar_show%tabs) lines = 1
            call select_frame_end(saved)
        else if (tab_bar_show%value) then
            lines = 1
        end if
    end function tab_bar_lines_for_frame

    !> Update the `tab-bar-lines' frame parameter of the selected frame,
    !> or with ALL of all frames as well as the default for new frames
    !> (`default_frame').  Port of tab-bar.el:tab-bar--update-tab-bar-lines
    !> (FRAMES absent or true).
    subroutine update_tab_bar_lines(all)
        logical, intent(in) :: all
        type(handle_type), allocatable :: frames(:)
        type(frame_type), pointer :: f
        integer :: k, lines

        call ensure_frame_windows()

        if (all) then
            frames = frame_list()
        else
            frames = [selected_frame%handle]
        end if

        do k = 1, size(frames)
            f => get_frame(frames(k))
            if (.not. associated(f)) cycle
            if (f%parameters%tab_bar_lines_keep_state) cycle
            lines = tab_bar_lines_for_frame(frames(k))
            f => get_frame(frames(k))
            call set_tab_bar_lines(f, lines)
        end do

        if (all) then
            default_frame%tab_bar_lines = 0

            if (minor_modes%tab_bar_mode%enabled() .and. tab_bar_show%tabs < 0) then
                if (tab_bar_show%value) default_frame%tab_bar_lines = 1
            end if
        end if
    end subroutine update_tab_bar_lines

    ! ------------------------------------------------------------------
    ! Tab Bar mode
    ! ------------------------------------------------------------------

    !> Toggle the tab bar in all graphical frames (Tab Bar mode).
    !>
    !> When this mode is enabled, Fortmacs displays a tab bar on top of each frame.
    !> The tab bar is a row of tabs -- buttons that you can click
    !> to switch the frame between different window configurations.
    !> See `current_window_configuration' for more about window configurations.
    !> To add a button (which can then record one more window configuration),
    !> click on the "+" button.  Clicking on the "x" icon of a button
    !> deletes the button.
    !>
    !> If you intend to use `tab_bar_mode' with `winner_mode', we recommend
    !> using `tab_bar_history_mode' instead, since it provides tab-specific window
    !> configuration history, and is better behaved when `tab_bar_mode'
    !> is turned on.  Port of tab-bar.el:tab-bar-mode.
    recursive subroutine tab_bar_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%tab_bar_mode%command(status)
    end subroutine tab_bar_mode

    !> The body of Tab Bar mode: recalculate `tab-bar-lines' for all
    !> frames, load the buttons and define or undefine the keys.  Port of
    !> tab-bar.el:tab-bar-mode (its body).
    recursive subroutine tab_bar_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call update_tab_bar_lines(.true.)

        if (enable) then
            call load_tab_bar_buttons()
            call define_tab_bar_keys()
        else
            call undefine_tab_bar_keys()
        end if
    end subroutine tab_bar_mode_body

    !> Load the icons for the tab buttons: on a terminal, their text.
    !> Port of tab-bar.el:tab-bar--load-buttons (the images and the menu
    !> bar's button wait for the GUI and the menu bar).
    subroutine load_tab_bar_buttons()
        tab_bar_new_button = " + "
        tab_bar_close_button = " x"
    end subroutine load_tab_bar_buttons

    !> The prefix of the keys `tab_bar_select_tab_modifiers' gives ("" for
    !> none): "M-" for meta, and so on.  Not a port.
    function select_tab_modifiers_prefix() result(prefix)
        character(len=:), allocatable :: prefix

        prefix = ""
        if (tab_bar_select_tab_modifiers%alt) prefix = prefix//"A-"
        if (tab_bar_select_tab_modifiers%control) prefix = prefix//"C-"
        if (tab_bar_select_tab_modifiers%hyper) prefix = prefix//"H-"
        if (tab_bar_select_tab_modifiers%meta) prefix = prefix//"M-"
        if (tab_bar_select_tab_modifiers%shift) prefix = prefix//"S-"
        if (tab_bar_select_tab_modifiers%super) prefix = prefix//"s-"
    end function select_tab_modifiers_prefix

    !> Whether KEY has a binding in the global map.  Port of
    !> subr.el:global-key-binding (as a truth value).
    logical function has_global_key_binding(key) result(bound)
        character(len=*), intent(in) :: key
        type(value_type), allocatable :: events(:)
        type(value_type) :: binding
        type(status_type) :: status

        bound = .false.
        call key_parse(key, events, status)
        if (.not. status%ok()) return
        call lookup_key(make_keymap_ref(current_global_map()), events, binding, status)
        if (.not. status%ok()) return
        bound = allocated(binding%a)
    end function has_global_key_binding

    !> Install key bindings to switch between tabs if so configured.
    !> Port of tab-bar.el:tab-bar--define-keys (`tab_bar_format_global' is
    !> Deferred, with its part).
    subroutine define_tab_bar_keys()
        character(len=:), allocatable :: prefix
        character(len=1) :: digit
        type(status_type) :: status
        integer :: i

        prefix = select_tab_modifiers_prefix()

        if ((tab_bar_define_keys%value .or. tab_bar_define_keys%choice == TAB_BAR_DEFINE_KEYS_NUMERIC) .and. &
            len(prefix) > 0) then
            call keymap_set(tab_bar_mode_map, prefix//"0", "recent_tab", status)

            do i = 1, 8
                write (digit, "(i1)") i
                call keymap_set(tab_bar_mode_map, prefix//digit, "select_tab_bar_tab", status)
            end do

            call keymap_set(tab_bar_mode_map, prefix//"9", "last_tab", status)
        end if

        if (tab_bar_define_keys%value .or. tab_bar_define_keys%choice == TAB_BAR_DEFINE_KEYS_TAB) then
            if (.not. has_global_key_binding("C-<tab>")) then
                call keymap_set(tab_bar_mode_map, "C-<tab>", "next_tab", status)
            end if

            if (.not. has_global_key_binding("C-S-<tab>")) then
                call keymap_set(tab_bar_mode_map, "C-S-<tab>", "previous_tab", status)
            end if

            if (.not. has_global_key_binding("C-S-<iso-lefttab>")) then
                call keymap_set(tab_bar_mode_map, "C-S-<iso-lefttab>", "previous_tab", status)
            end if
        end if
    end subroutine define_tab_bar_keys

    !> Remove the binding of KEY from `tab_bar_mode_map' (define-key with
    !> REMOVE).  Not a port.
    subroutine remove_tab_bar_key(key)
        character(len=*), intent(in) :: key
        type(value_type), allocatable :: events(:)
        type(status_type) :: status

        call key_parse(key, events, status)
        if (.not. status%ok()) return
        call define_key(tab_bar_mode_map, events, value_type(), status, remove=.true.)
    end subroutine remove_tab_bar_key

    !> Uninstall key bindings previously bound by `define_tab_bar_keys'.
    !> Port of tab-bar.el:tab-bar--undefine-keys.
    subroutine undefine_tab_bar_keys()
        character(len=:), allocatable :: prefix
        character(len=1) :: digit
        integer :: i

        prefix = select_tab_modifiers_prefix()

        if (len(prefix) > 0) then
            do i = 0, 9
                write (digit, "(i1)") i
                call remove_tab_bar_key(prefix//digit)
            end do
        end if

        call remove_tab_bar_key("C-<tab>")
        call remove_tab_bar_key("C-S-<tab>")
        call remove_tab_bar_key("C-S-<iso-lefttab>")
    end subroutine undefine_tab_bar_keys

    ! ------------------------------------------------------------------
    ! The tab bar's items
    ! ------------------------------------------------------------------

    !> Separator between tabs: `tab_bar_separator', else a space on a
    !> window system and "|" on a terminal.  Port of
    !> tab-bar.el:tab-bar-separator.
    function get_tab_bar_separator() result(separator)
        character(len=:), allocatable :: separator

        if (allocated(tab_bar_separator)) then
            separator = tab_bar_separator
        else if (associated(selected_frame%rif)) then
            separator = " "
        else
            separator = "|"
        end if
    end function get_tab_bar_separator

    !> Append an item KEY whose caption is TEXT in FACE (if present and not
    !> empty) to ITEMS.  Not a port.
    subroutine append_item(items, key, text, face)
        type(tab_bar_item_type), allocatable, intent(inout) :: items(:)
        character(len=*), intent(in) :: key, text
        character(len=*), intent(in), optional :: face
        type(tab_bar_item_type) :: item

        if (.not. allocated(items)) allocate (items(0))
        item%key = key
        allocate (item%runs(1))
        item%runs(1)%text = text
        if (present(face)) item%runs(1)%faces(1) = face
        items = [items, item]
    end subroutine append_item

    !> The separator as a tab bar item: an element of `tab_bar_format'.
    !> Port of tab-bar.el:tab-bar-separator (as an element of
    !> tab-bar-format).
    subroutine format_tab_bar_separator(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)

        call append_item(items, "", get_tab_bar_separator())
    end subroutine format_tab_bar_separator

    !> Produce back and forward buttons for the tab bar.
    !> These buttons will be shown when `tab_bar_history_mode' is enabled.
    !> You can hide these buttons by customizing `tab_bar_format' and removing
    !> `format_tab_bar_history' from it.  Port of
    !> tab-bar.el:tab-bar-format-history.
    subroutine format_tab_bar_history(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)

        if (.not. minor_modes%tab_bar_history_mode%enabled()) return
        call append_item(items, "sep-history-back", get_tab_bar_separator())
        call append_item(items, "history-back", tab_bar_back_button)
        call append_item(items, "sep-history-forward", get_tab_bar_separator())
        call append_item(items, "history-forward", tab_bar_forward_button)
    end subroutine format_tab_bar_history

    !> The face of TAB on the tab bar: `tab_bar_tab' for the current tab,
    !> else `tab_bar_tab_inactive'; with FACES GROUP_FACES, a tab that is
    !> neither current nor in a group has `tab_bar_tab_ungrouped'.
    !> Port of tab-bar.el:tab-bar-tab-face-default and
    !> tab-bar.el:tab-bar-tab-group-face-default.
    function tab_face(tab, faces) result(face)
        type(tab_type), intent(in) :: tab
        integer, intent(in) :: faces
        character(len=:), allocatable :: face

        if (faces == GROUP_FACES .and. .not. (tab%current .or. allocated(tab%group))) then
            face = "tab_bar_tab_ungrouped"
        else if (tab%current) then
            face = "tab_bar_tab"
        else
            face = "tab_bar_tab_inactive"
        end if
    end function tab_face

    !> Format TAB, the Ith tab, and append its items to ITEMS: a separator
    !> and the tab, its name with the hint and close button in its face.
    !> Port of tab-bar.el:tab-bar--format-tab with
    !> tab-bar.el:tab-bar-tab-name-format-default and the default
    !> `tab-bar-tab-name-format-functions' (tab-bar-tab-name-format-hints,
    !> tab-bar-tab-name-format-close-button, tab-bar-tab-name-format-face;
    !> the mouse face waits for the mouse).
    subroutine format_tab(tab, i, faces, items)
        type(tab_type), intent(in) :: tab
        integer, intent(in) :: i, faces
        type(tab_bar_item_type), allocatable, intent(inout) :: items(:)
        type(tab_bar_item_type) :: item
        character(len=32) :: key
        character(len=24) :: hint
        logical :: close_button

        write (key, '("sep-", i0)') i
        call append_item(items, trim(key), get_tab_bar_separator())

        if (tab%current) then
            item%key = "current-tab"
        else
            write (item%key, '("tab-", i0)') i
        end if

        close_button = .not. tab_bar_close_button_show%is_off() .and. allocated(tab_bar_close_button)

        if (close_button) then
            if (tab%current) then
                close_button = tab_bar_close_button_show%choice /= TAB_BAR_CLOSE_BUTTON_SHOW_NON_SELECTED
            else
                close_button = tab_bar_close_button_show%choice /= TAB_BAR_CLOSE_BUTTON_SHOW_SELECTED
            end if
        end if

        if (close_button) then
            allocate (item%runs(2))
            item%runs(2)%text = tab_bar_close_button
            item%runs(2)%close_tab = .true.
            item%runs(2)%faces(1) = tab_face(tab, faces)
        else
            allocate (item%runs(1))
        end if

        item%runs(1)%text = tab%name

        if (tab_bar_tab_hints) then
            write (hint, "(i0)") i
            item%runs(1)%text = trim(hint)//" "//tab%name
        end if

        item%runs(1)%faces(1) = tab_face(tab, faces)
        items = [items, item]
    end subroutine format_tab

    !> Produce all the tabs for the tab bar.  Port of
    !> tab-bar.el:tab-bar-format-tabs.
    subroutine format_tab_bar_tabs(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)
        integer :: i

        call update_tab_bar_tabs()
        allocate (items(0))

        do i = 1, tab_count()
            call format_tab(selected_frame%parameters%tabs(i), i, TAB_FACES, items)
        end do
    end subroutine format_tab_bar_tabs

    !> Format TAB, the Ith tab, as a tab that represents a group of tabs
    !> (CURRENT when it is the current group) and append its items to
    !> ITEMS.  Port of tab-bar.el:tab-bar--format-tab-group with
    !> tab-bar.el:tab-bar-tab-group-format-default.
    subroutine format_tab_group(tab, i, current, items)
        type(tab_type), intent(in) :: tab
        integer, intent(in) :: i
        logical, intent(in) :: current
        type(tab_bar_item_type), allocatable, intent(inout) :: items(:)
        character(len=32) :: key
        character(len=24) :: hint
        character(len=:), allocatable :: group

        write (key, '("sep-", i0)') i
        call append_item(items, trim(key), get_tab_bar_separator())
        group = ""
        if (allocated(tab%group)) group = tab%group

        if (tab_bar_tab_hints .and. .not. current .and. .not. tab_bar_show_inactive_group_tabs) then
            write (hint, "(i0)") i
            group = trim(hint)//" "//group
        end if

        if (current) then
            call append_item(items, "current-group", group, "tab_bar_tab_group_current")
        else
            write (key, '("group-", i0)') i
            call append_item(items, trim(key), group, "tab_bar_tab_group_inactive")
        end if
    end subroutine format_tab_group

    !> Whether groups A and B (unallocated for none) are equal.
    !> Not a port (`equal' on two strings or unallocated ones).
    logical function is_same_group(a, b)
        character(len=:), allocatable, intent(in) :: a, b

        if (.not. allocated(a) .or. .not. allocated(b)) then
            is_same_group = .not. allocated(a) .and. .not. allocated(b)
        else
            is_same_group = a == b .and. len(a) == len(b)
        end if
    end function is_same_group

    !> Produce tabs for the tab bar grouped according to their groups.
    !> Port of tab-bar.el:tab-bar-format-tabs-groups.
    subroutine format_tab_bar_tabs_groups(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)
        character(len=:), allocatable :: current_group, previous_group, tab_group
        integer :: i, k
        logical :: grouped, new_group

        call update_tab_bar_tabs()
        allocate (items(0))
        k = current_tab_index()

        if (k > 0) then
            if (allocated(selected_frame%parameters%tabs(k)%group)) current_group = selected_frame%parameters%tabs(k)%group
        end if

        do i = 1, tab_count()
            associate (tab => selected_frame%parameters%tabs(i))
                if (allocated(tab_group)) deallocate (tab_group)
                if (allocated(tab%group)) tab_group = tab%group
                grouped = allocated(tab_group)
                new_group = .not. is_same_group(previous_group, tab_group)

                if (is_same_group(tab_group, current_group) .or. .not. grouped) then
                    ! Show current group tabs and ungrouped tabs.
                    if (new_group .and. grouped) then
                        ! Prepend current group name before first tab.
                        call format_tab_group(tab, i, .true., items)
                    end if

                    ! Override default tab faces to use group faces.
                    call format_tab(tab, i, GROUP_FACES, items)
                else if (new_group) then
                    ! Show first tab of other groups with a group name.
                    call format_tab_group(tab, i, .false., items)
                    if (tab_bar_show_inactive_group_tabs) call format_tab(tab, i, GROUP_FACES, items)
                else
                    ! Hide other group tabs.
                    if (tab_bar_show_inactive_group_tabs) call format_tab(tab, i, GROUP_FACES, items)
                end if

                if (allocated(previous_group)) deallocate (previous_group)
                if (allocated(tab_group)) previous_group = tab_group
            end associate
        end do
    end subroutine format_tab_bar_tabs_groups

    !> Button to add a new tab.  Port of tab-bar.el:tab-bar-format-add-tab.
    subroutine format_tab_bar_add_tab(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)

        if (tab_bar_new_button_show .and. allocated(tab_bar_new_button)) then
            call append_item(items, "add-tab", tab_bar_new_button)
        end if
    end subroutine format_tab_bar_add_tab

    !> The items of the tab bar: those of each procedure of
    !> `tab_bar_format' in turn, an item without a key keyed `string-I' (I
    !> the procedure's place), their widths made even by
    !> `tab_bar_auto_width'.  Port of tab-bar.el:tab-bar-make-keymap-1 with
    !> tab-bar.el:tab-bar-format-list (the keymap and the mouse bindings
    !> of `tab_bar_map' wait for the mouse).
    subroutine make_tab_bar_items(items)
        type(tab_bar_item_type), allocatable, intent(out) :: items(:)
        type(tab_bar_item_type), allocatable :: part(:)
        integer :: i, k

        allocate (items(0))
        if (.not. allocated(tab_bar_format%entries)) return

        do i = 1, size(tab_bar_format%entries)
            call tab_bar_format%entries(i)%item(part)
            if (.not. allocated(part)) cycle

            do k = 1, size(part)
                if (len_trim(part(k)%key) == 0) write (part(k)%key, '("string-", i0)') i
                items = [items, part(k)]
            end do
        end do

        if (tab_bar_auto_width) call auto_width_tab_bar_items(items)
    end subroutine make_tab_bar_items

    !> The runs of text of the selected frame's tab bar, for redisplay
    !> (display_engine_mod's `get_tab_bar_items').  Port of keyboard.c:tab_bar_items
    !> (the captions of the items).
    subroutine get_tab_bar_runs(runs)
        type(tab_bar_run_type), allocatable, intent(out) :: runs(:)
        type(tab_bar_item_type), allocatable :: items(:)
        integer :: k, j

        call make_tab_bar_items(items)
        allocate (runs(0))

        do k = 1, size(items)
            do j = 1, size(items(k)%runs)
                runs = [runs, items(k)%runs(j)]
            end do
        end do
    end subroutine get_tab_bar_runs

    ! ------------------------------------------------------------------
    ! Automatic tab widths
    ! ------------------------------------------------------------------

    !> Whether ITEM is a tab or a tab group whose width is made even.
    !> Port of tab-bar.el:tab-bar-auto-width-predicate-default (the
    !> default `tab-bar-auto-width-faces').
    logical function is_auto_width_item(item)
        type(tab_bar_item_type), intent(in) :: item

        is_auto_width_item = index(item%key, "current-tab") == 1 .or. index(item%key, "group-") == 1 .or. &
                             index(item%key, "tab-") == 1
    end function is_auto_width_item

    !> The text of the runs of a caption.  Not a port.
    function caption_text(runs) result(text)
        type(tab_bar_run_type), intent(in) :: runs(:)
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, size(runs)
            text = text//runs(k)%text
        end do
    end function caption_text

    !> The characters of a caption, with their faces and `close-tab'
    !> property.  Not a port.
    subroutine caption_chars(runs, chars)
        type(tab_bar_run_type), intent(in) :: runs(:)
        type(caption_char_type), allocatable, intent(out) :: chars(:)
        type(caption_char_type) :: c
        integer(int64) :: pos
        integer(int32) :: ch, length
        integer :: k

        allocate (chars(0))

        do k = 1, size(runs)
            pos = 1

            do while (pos <= len(runs(k)%text, kind=int64))
                call string_char_and_length(runs(k)%text, pos, ch, length)
                length = max(length, 1_int32)
                c%text = runs(k)%text(pos:pos + length - 1)
                c%faces = runs(k)%faces
                c%close_tab = runs(k)%close_tab
                chars = [chars, c]
                pos = pos + length
            end do
        end do
    end subroutine caption_chars

    !> The runs of characters CHARS, those with the same faces and
    !> `close-tab' property joined.  Not a port.
    subroutine chars_caption(chars, runs)
        type(caption_char_type), intent(in) :: chars(:)
        type(tab_bar_run_type), allocatable, intent(out) :: runs(:)
        type(tab_bar_run_type) :: run
        integer :: k, n

        allocate (runs(0))
        n = 0

        do k = 1, size(chars)
            if (n > 0) then
                if (all(runs(n)%faces == chars(k)%faces) .and. (runs(n)%close_tab .eqv. chars(k)%close_tab)) then
                    runs(n)%text = runs(n)%text//chars(k)%text
                    cycle
                end if
            end if

            run%text = chars(k)%text
            run%faces = chars(k)%faces
            run%close_tab = chars(k)%close_tab
            runs = [runs, run]
            n = n + 1
        end do
    end subroutine chars_caption

    !> The width in columns of the characters CHARS.  Not a port
    !> (string-pixel-width on a terminal).
    integer function chars_width(chars) result(width)
        type(caption_char_type), intent(in) :: chars(:)
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, size(chars)
            text = text//chars(k)%text
        end do

        width = int(string_width(text))
    end function chars_width

    !> Return tab-bar items with resized tab names: the names of the tabs
    !> and tab groups are padded with spaces or truncated (the last two
    !> characters kept then shown in the `shadow' face) to share the width
    !> of the frame left by the other items, between 2 and 20 columns on a
    !> terminal.  Port of tab-bar.el:tab-bar-auto-width (with the default
    !> `tab-bar-auto-width-max' and `tab-bar-auto-width-min'; the
    !> memoization is left out).
    subroutine auto_width_tab_bar_items(items)
        type(tab_bar_item_type), intent(inout) :: items(:)
        type(caption_char_type), allocatable :: chars(:), longer(:)
        type(caption_char_type) :: space
        integer :: k, ntabs, width, prev_width, ins_pos, pos, j, n
        logical :: close_p
        character(len=:), allocatable :: non_tabs

        ntabs = 0
        non_tabs = ""

        do k = 1, size(items)
            if (is_auto_width_item(items(k))) then
                ntabs = ntabs + 1
            else if (items(k)%key /= "align-right") then
                non_tabs = non_tabs//caption_text(items(k)%runs)
            end if
        end do

        if (ntabs == 0) return
        width = (frame_cols(selected_frame) - int(string_width(non_tabs)))/ntabs
        width = max(width, 2)
        width = min(width, 20)

        do k = 1, size(items)
            if (.not. is_auto_width_item(items(k))) cycle
            call caption_chars(items(k)%runs, chars)
            n = size(chars)
            if (n == 0) cycle
            close_p = chars(n)%close_tab
            prev_width = chars_width(chars)

            if (prev_width < width) then
                space%text = " "
                space%faces = chars(1)%faces
                space%close_tab = chars(1)%close_tab
                ins_pos = n
                if (close_p) ins_pos = n - int(count_characters(tab_bar_close_button))

                do
                    longer = [chars(1:ins_pos), space, chars(ins_pos + 1:)]
                    if (chars_width(longer) >= width) exit
                    call move_alloc(longer, chars)
                end do
            else if (prev_width > width) then
                do
                    n = size(chars)
                    if (n < 2) exit

                    if (close_p) then
                        chars = [chars(1:n - 2), chars(n:n)]
                    else
                        chars = chars(1:n - 1)
                    end if

                    if (chars_width(chars) <= width) exit
                end do

                n = size(chars)
                pos = n
                if (close_p) pos = n - 1

                do j = max(0, pos - 2) + 1, max(0, pos)
                    chars(j)%faces = [character(len=40) :: "shadow", chars(j)%faces(1:2)]
                end do
            end if

            call chars_caption(chars, items(k)%runs)
        end do
    end subroutine auto_width_tab_bar_items

    ! ------------------------------------------------------------------
    ! Selecting tabs
    ! ------------------------------------------------------------------

    !> Switch to the tab by its absolute position TAB_NUMBER in the tab bar.
    !> When this command is bound to a numeric key (with a key prefix or modifier key
    !> using `tab_bar_select_tab_modifiers'), calling it without an argument
    !> will translate its bound numeric key to the numeric argument.
    !> Also the prefix argument TAB_NUMBER can be used to override
    !> the numeric key, so it takes precedence over the bound digit key.
    !> For example, `<MODIFIER>-2' will select the second tab, but `C-u 15
    !> <MODIFIER>-2' will select the 15th tab.  TAB_NUMBER counts from 1.
    !> Negative TAB_NUMBER counts tabs from the end of the tab bar.
    !> Port of tab-bar.el:tab-bar-select-tab (`tab_bar_select_restore_windows',
    !> `tab_bar_select_restore_context', the hook
    !> `tab_bar_tab_post_select_functions' and switching from an active
    !> minibuffer, tab-bar-minibuffer-restore-tab, are Deferred).
    recursive subroutine select_tab(status, tab_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number
        type(tab_type) :: from_tab, to_tab
        type(handle_type), allocatable :: seen(:), buried(:)
        type(handle_type) :: b
        character(len=:), allocatable :: to_name
        integer(int64) :: key, pos
        integer :: number, n, from_index, to_number, to_index
        logical :: minibuffer_was_active

        status = status_type()

        if (present(tab_number)) then
            number = tab_number
        else
            number = 0

            if (vtag(last_command_event) == V_INTEGER) then
                key = iand(xfixnum(last_command_event), not(int(CHAR_MODIFIER_MASK, int64)))
                if (key >= iachar("1") .and. key <= iachar("9")) number = int(key) - iachar("0")
            end if
        end if

        call update_tab_bar_tabs()
        n = tab_count()
        from_index = current_tab_index()

        if (number < 0) then
            to_number = n + 1 + number
        else if (number == 0) then
            to_number = from_index
        else
            to_number = number
        end if

        to_index = max(1, min(to_number, n))
        minibuffer_was_active = selected_frame%minibuffer_selected .and. minibuf_level > 0
        if (from_index == to_index) return
        call make_tab_from_current(from_tab)

        associate (wc => selected_frame%parameters%tabs(to_index))
            to_name = wc%name

            if (wc%has_window_configuration) then
                call set_window_configuration(wc%window_configuration%window_configuration)

                ! set_window_configuration does not restore the value of
                ! point in the current buffer, so restore it separately.
                if (wc%window_configuration%point%index /= 0) then
                    call marker_buffer(wc%window_configuration%point, b, status)

                    if (status%ok() .and. is_buffer_live(b)) then
                        call marker_position(wc%window_configuration%point, pos, status)
                        if (status%ok()) call goto_char(pos)
                    end if

                    status = status_type()
                end if

                if (allocated(wc%buffers)) then
                    seen = wc%buffers
                else
                    allocate (seen(0))
                end if

                if (allocated(wc%buried_buffers)) then
                    buried = wc%buried_buffers
                else
                    allocate (buried(0))
                end if

                call exchange_frame_buffers(seen, buried)

                if (minor_modes%tab_bar_history_mode%enabled()) then
                    call destroy_history(selected_frame%tab_history_back)
                    call destroy_history(selected_frame%tab_history_forward)
                    call restore_history(wc%has_history, wc%history_back, selected_frame%tab_history_back)
                    call restore_history(wc%has_history, wc%history_forward, selected_frame%tab_history_forward)
                end if
            end if
        end associate

        ! Select the minibuffer when it was active before switching tabs.
        if (minibuffer_was_active .and. minibuf_level > 0) selected_frame%minibuffer_selected = .true.
        if (minor_modes%tab_bar_history_mode%enabled()) tab_bar_history_omit = .true.
        if (from_index > 0) selected_frame%parameters%tabs(from_index) = from_tab
        call make_current_tab(to_tab, selected_frame%parameters%tabs(to_index))
        call selected_frame%parameters%tabs(to_index)%destroy()
        selected_frame%parameters%tabs(to_index) = to_tab

        if (.not. minor_modes%tab_bar_mode%enabled()) then
            call tab_message(format_message("Selected tab '")//to_name//format_message("'"))
        end if
    end subroutine select_tab

    !> Copies of the window configurations FROM of a tab's history in TO
    !> when HAS (the tab kept its history) and the first one is one, else
    !> none.  Not a port (tab-bar-select-tab's puthash of the history).
    subroutine restore_history(has, from, to)
        logical, intent(in) :: has
        type(tab_window_configuration_type), allocatable, intent(in) :: from(:)
        type(tab_window_configuration_type), allocatable, intent(out) :: to(:)

        if (has .and. allocated(from)) then
            if (size(from) > 0) then
                if (allocated(from(1)%window_configuration%saved)) then
                    call copy_history(from, to)
                    return
                end if
            end if
        end if

        allocate (to(0))
    end subroutine restore_history

    !> Switch to ARGth next tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-next-tab.
    recursive subroutine switch_to_next_tab(status, arg)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg
        integer :: count, from_index

        count = 1
        if (present(arg)) count = arg
        call update_tab_bar_tabs()
        from_index = max(current_tab_index(), 1) - 1
        call select_tab(status, modulo(from_index + count, tab_count()) + 1)
    end subroutine switch_to_next_tab

    !> Switch to ARGth previous tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-prev-tab.
    recursive subroutine switch_to_previous_tab(status, arg)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg
        integer :: count

        count = 1
        if (present(arg)) count = arg
        call switch_to_next_tab(status, -count)
    end subroutine switch_to_previous_tab

    !> Switch to the last tab or ARGth tab from the end of the tab bar.
    !> Interactively, ARG is the prefix numeric argument; it defaults to 1,
    !> which means the last tab on the tab bar.  For example, `C-u 2
    !> <MODIFIER>-9' selects the tab before the last tab.  Port of
    !> tab-bar.el:tab-bar-switch-to-last-tab.
    recursive subroutine switch_to_last_tab(status, arg)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg
        integer :: count

        count = 1
        if (present(arg)) count = arg
        call update_tab_bar_tabs()
        call select_tab(status, tab_count() - (abs(count) - 1))
    end subroutine switch_to_last_tab

    !> Switch to ARGth most recently visited tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-recent-tab.
    recursive subroutine switch_to_recent_tab(status, arg)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg
        integer :: count, k

        status = status_type()
        count = 1
        if (present(arg)) count = arg
        call update_tab_bar_tabs()
        k = tab_index_recent(count)

        if (k > 0) then
            call select_tab(status, k)
        else
            call tab_message("No more recent tabs")
        end if
    end subroutine switch_to_recent_tab

    !> Switch to the tab by NAME.
    !> Default values are tab names sorted by recency, so you can use
    !> \<minibuffer_local_map>\[next_history_element]
    !> to get the name of the most recently visited tab, the second
    !> most recent, and so on.
    !> When the tab with that NAME doesn't exist, create a new tab
    !> and rename it to NAME.  Port of tab-bar.el:tab-bar-switch-to-tab.
    recursive subroutine switch_to_tab(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        call update_tab_bar_tabs()
        k = tab_index_by_name(name)

        if (k > 0) then
            call select_tab(status, k)
        else
            call make_tab(status)
            if (.not. status%ok()) return
            call rename_tab(name, status)
        end if
    end subroutine switch_to_tab

    ! ------------------------------------------------------------------
    ! Moving tabs
    ! ------------------------------------------------------------------

    !> Move tab from FROM_NUMBER position to new position at TO_NUMBER.
    !> FROM_NUMBER defaults to the current tab number.
    !> FROM_NUMBER and TO_NUMBER count from 1.
    !> Negative TO_NUMBER counts tabs from the end of the tab bar.
    !> Argument addressing is absolute in contrast to `move_tab'
    !> where argument addressing is relative.  Port of
    !> tab-bar.el:tab-bar-move-tab-to.
    subroutine move_tab_to(to_number, status, from_number)
        integer, intent(in) :: to_number
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: from_number
        type(tab_type) :: from_tab
        integer :: from, to, n, to_index

        status = status_type()
        call update_tab_bar_tabs()
        n = tab_count()
        from = current_tab_index()
        if (present(from_number)) from = from_number
        if (from < 1 .or. from > n) return
        to = to_number
        if (to < 0) to = n + 1 + to
        to_index = max(0, min(to - 1, n - 1))
        from_tab = selected_frame%parameters%tabs(from)
        call remove_tab(from)
        call insert_tab(from_tab, to_index + 1)
    end subroutine move_tab_to

    !> Move the current tab ARG positions to the right.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> If ARG is negative, move the current tab ARG positions to the left.
    !> Argument addressing is relative in contrast to `move_tab_to',
    !> where argument addressing is absolute.  Port of
    !> tab-bar.el:tab-bar-move-tab.
    subroutine move_tab(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: from_index, to_index

        call update_tab_bar_tabs()
        from_index = max(current_tab_index(), 1) - 1
        to_index = modulo(from_index + arg, tab_count())
        call move_tab_to(to_index + 1, status, from_index + 1)
    end subroutine move_tab

    !> Move the current tab ARG positions to the left.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Like `move_tab', but moves in the opposite direction.  Port of
    !> tab-bar.el:tab-bar-move-tab-backward.
    subroutine move_tab_backward(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status

        call move_tab(-arg, status)
    end subroutine move_tab_backward

    ! ------------------------------------------------------------------
    ! Making tabs
    ! ------------------------------------------------------------------

    !> Add a new tab at the absolute position TAB_NUMBER.
    !> TAB_NUMBER counts from 1.  If no TAB_NUMBER is specified, then add
    !> a new tab at the position specified by `tab_bar_new_tab_to'.
    !> Negative TAB_NUMBER counts tabs from the end of the tab bar,
    !> and -1 means the new tab will become the last one.
    !> Argument addressing is absolute in contrast to `make_tab',
    !> where argument addressing is relative.  Port of
    !> tab-bar.el:tab-bar-new-tab-to (the hook
    !> `tab_bar_tab_post_open_functions' and side windows are Deferred).
    recursive subroutine make_tab_to(status, tab_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number
        type(tab_type) :: from_tab, to_tab, template
        type(window_state_type) :: state
        type(handle_type) :: new, window, buffer
        integer(int64) :: bi
        integer :: n, from_index, to_index
        logical :: saved_ignore

        status = status_type()
        call update_tab_bar_tabs()
        n = tab_count()
        from_index = current_tab_index()
        call make_tab_from_current(from_tab)

        if (.not. tab_bar_new_tab_choice%is_off()) then
            ! Handle the case when it's called in the active minibuffer.
            if (selected_frame%minibuffer_selected) then
                selected_frame%minibuffer_selected = .false.
                window = get_mru_window(.false., .false.)
                if (window%index /= 0) call select_window(window, status)
                status = status_type()
            end if

            saved_ignore = ignore_window_parameters
            ignore_window_parameters = .true.

            if (tab_bar_new_tab_choice%choice == TAB_BAR_NEW_TAB_CHOICE_CLONE) then
                ! Create new unique windows with the same layout.
                call get_window_state(state)
                call set_window_state(state, status)
                call destroy_window_state(state)
            else
                call delete_other_windows(selected_frame%selected_window, status)

                if (status%ok()) then
                    if (tab_bar_new_tab_choice%choice == TAB_BAR_NEW_TAB_CHOICE_WINDOW) then
                        ! Create new unique window from remaining window.
                        windows(selected_frame%selected_window%index)%window_side = WINDOW_SIDE_NONE
                        call get_window_state(state)
                        call set_window_state(state, status)
                        call destroy_window_state(state)
                    else
                        ! Create a new window to get rid of old window
                        ! parameters (e.g. prev/next buffers) of old window.
                        window = selected_frame%selected_window
                        call split_window(window, window_safe_min_width, .true., .true., new, status)
                        if (status%ok()) call delete_window(window, status)
                    end if
                end if
            end if

            ignore_window_parameters = saved_ignore

            if (.not. status%ok()) then
                call from_tab%destroy()
                return
            end if

            buffer = null_handle()

            if (associated(tab_bar_new_tab_choice%procedure)) then
                call tab_bar_new_tab_choice%procedure(buffer)
            else if (allocated(tab_bar_new_tab_choice%text)) then
                buffer = get_buffer(tab_bar_new_tab_choice%text)
                if (.not. is_buffer_live(buffer)) call find_file_noselect(tab_bar_new_tab_choice%text, buffer, status)
            end if

            if (.not. status%ok()) then
                call from_tab%destroy()
                return
            end if

            if (is_buffer_live(buffer)) then
                call resolve_buffer(buffer, bi, status)
                if (status%ok()) call switch_to_buffer(buffers(bi)%name, status)

                if (.not. status%ok()) then
                    call from_tab%destroy()
                    return
                end if
            end if
        end if

        if (from_index > 0) selected_frame%parameters%tabs(from_index) = from_tab

        if (tab_bar_new_tab_group%value .and. .not. associated(tab_bar_new_tab_group%procedure) .and. &
            .not. allocated(tab_bar_new_tab_group%text)) then
            if (allocated(from_tab%group)) template%group = from_tab%group
            call make_current_tab(to_tab, template)
        else
            call make_current_tab(to_tab)
        end if

        if (present(tab_number)) then
            if (tab_number < 0) then
                to_index = n + 1 + tab_number
            else
                to_index = tab_number - 1
            end if
        else if (associated(tab_bar_new_tab_to%procedure)) then
            to_index = tab_bar_new_tab_to%procedure()
        else
            select case (tab_bar_new_tab_to%choice)
            case (TAB_BAR_NEW_TAB_TO_LEFTMOST)
                to_index = 0
            case (TAB_BAR_NEW_TAB_TO_RIGHTMOST)
                to_index = n
            case (TAB_BAR_NEW_TAB_TO_LEFT)
                to_index = 1
                if (from_index > 0) to_index = from_index - 1
            case default
                to_index = 1
                if (from_index > 0) to_index = from_index
            end select
        end if

        to_index = max(0, min(to_index, n))
        call insert_tab(to_tab, to_index + 1)

        if (minor_modes%tab_bar_history_mode%enabled()) then
            call destroy_history(selected_frame%tab_history_back)
            call destroy_history(selected_frame%tab_history_forward)
            tab_bar_history_omit = .true.
        end if

        if (.not. tab_bar_show%is_off()) then
            if (.not. minor_modes%tab_bar_mode%enabled()) then
                ! Turn on `tab_bar_mode' since a tab was created.
                ! Note: this also updates `tab-bar-lines'.
                call minor_modes%tab_bar_mode%enable(status)
                if (.not. status%ok()) return
            else
                call update_tab_bar_lines(.false.)
            end if
        end if

        if (.not. minor_modes%tab_bar_mode%enabled()) then
            call tab_message("Added new tab at "//new_tab_to_text())
        end if
    end subroutine make_tab_to

    !> The value of `tab_bar_new_tab_to' as a message shows it: its named
    !> constant, or that a procedure is set.  Not a port.
    function new_tab_to_text() result(text)
        character(len=:), allocatable :: text

        if (associated(tab_bar_new_tab_to%procedure)) then
            text = "a procedure set by the configuration"
            return
        end if

        select case (tab_bar_new_tab_to%choice)
        case (TAB_BAR_NEW_TAB_TO_LEFTMOST)
            text = "TAB_BAR_NEW_TAB_TO_LEFTMOST"
        case (TAB_BAR_NEW_TAB_TO_LEFT)
            text = "TAB_BAR_NEW_TAB_TO_LEFT"
        case (TAB_BAR_NEW_TAB_TO_RIGHTMOST)
            text = "TAB_BAR_NEW_TAB_TO_RIGHTMOST"
        case default
            text = "TAB_BAR_NEW_TAB_TO_RIGHT"
        end select
    end function new_tab_to_text

    !> Create a new tab ARG positions to the right.
    !> If a negative ARG, create a new tab ARG positions to the left.
    !> If ARG is zero, create a new tab in place of the current tab.
    !> If no ARG is specified, then add a new tab at the position
    !> specified by `tab_bar_new_tab_to'.
    !> Argument addressing is relative in contrast to `make_tab_to',
    !> where argument addressing is absolute.
    !> If FROM_NUMBER is a tab number, a new tab is created from that tab.
    !> Port of tab-bar.el:tab-bar-new-tab.
    recursive subroutine make_tab(status, arg, from_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg, from_number
        logical :: saved
        integer :: from_index

        status = status_type()

        if (present(from_number)) then
            saved = inhibit_tab_messages
            inhibit_tab_messages = .true.
            call select_tab(status, from_number)
            inhibit_tab_messages = saved
            if (.not. status%ok()) return
        end if

        if (present(arg)) then
            call update_tab_bar_tabs()
            from_index = max(current_tab_index(), 1) - 1
            call make_tab_to(status, from_index + arg + 1)
        else
            call make_tab_to(status)
        end if
    end subroutine make_tab

    !> Clone the current tab to ARG positions to the right.
    !> ARG and FROM_NUMBER have the same meaning as in `make_tab'.
    !> Port of tab-bar.el:tab-bar-duplicate-tab.
    recursive subroutine duplicate_tab(status, arg, from_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg, from_number
        type(tab_bar_new_tab_choice_choice_setting_type) :: saved_choice
        type(tab_bar_new_tab_group_choice_setting_type) :: saved_group

        saved_choice = tab_bar_new_tab_choice
        saved_group = tab_bar_new_tab_group
        tab_bar_new_tab_choice = TAB_BAR_NEW_TAB_CHOICE_CLONE
        tab_bar_new_tab_group = .true.
        call make_tab(status, arg, from_number)
        tab_bar_new_tab_choice = saved_choice
        tab_bar_new_tab_group = saved_group
    end subroutine duplicate_tab

    !> Move the selected window to a new tab.
    !> This command removes the selected window from the configuration stored
    !> on the current tab, and makes a new tab with that window in its
    !> configuration.  Port of tab-bar.el:tab-bar-move-window-to-tab.
    recursive subroutine move_window_to_tab(status)
        type(status_type), intent(out) :: status
        type(tab_bar_new_tab_choice_choice_setting_type) :: saved_choice
        logical :: saved_ignore

        saved_choice = tab_bar_new_tab_choice
        tab_bar_new_tab_choice = TAB_BAR_NEW_TAB_CHOICE_WINDOW
        call make_tab(status)
        tab_bar_new_tab_choice = saved_choice
        if (.not. status%ok()) return
        call switch_to_recent_tab(status)
        if (.not. status%ok()) return
        saved_ignore = ignore_window_parameters
        ignore_window_parameters = .true.
        call delete_window(selected_frame%selected_window, status)
        ignore_window_parameters = saved_ignore
        if (.not. status%ok()) return
        call switch_to_recent_tab(status)
    end subroutine move_window_to_tab

    ! ------------------------------------------------------------------
    ! Closing tabs
    ! ------------------------------------------------------------------

    !> Record tab K (from 1) of the selected frame, closed, in
    !> `tab_bar_closed_tabs' (the current tab as `make_tab_from_current'
    !> makes it), and take it out of the frame's tabs.  Not a port.
    subroutine push_closed_tab(k)
        integer, intent(in) :: k
        type(tab_type) :: tab

        if (selected_frame%parameters%tabs(k)%current) then
            call make_tab_from_current(tab)
            call record_closed_tab(tab, k)
        else
            call record_closed_tab(selected_frame%parameters%tabs(k), k)
        end if

        call remove_tab(k)
    end subroutine push_closed_tab

    !> Close the tab specified by its absolute position TAB_NUMBER.
    !> If no TAB_NUMBER is specified, then close the current tab and switch
    !> to the tab specified by `tab_bar_close_tab_select'.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and defaults to 1.
    !> TAB_NUMBER counts from 1.
    !> Optional TO_NUMBER could be specified to override the value of
    !> `tab_bar_close_tab_select' programmatically with a position
    !> of an existing tab to select after closing the current tab.
    !> TO_NUMBER counts from 1.
    !>
    !> The base behavior for the last tab on a frame is determined by
    !> `tab_bar_close_last_tab_choice'.  Port of tab-bar.el:tab-bar-close-tab
    !> (the hooks `tab_bar_tab_prevent_close_functions' and
    !> `tab_bar_tab_pre_close_functions' are Deferred).
    recursive subroutine close_tab(status, tab_number, to_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number, to_number
        type(tab_type) :: tab
        integer :: n, current_index, close_index, to_index
        logical :: saved

        status = status_type()
        call update_tab_bar_tabs()
        n = tab_count()
        current_index = current_tab_index()
        close_index = current_index
        if (present(tab_number)) close_index = tab_number

        if (n == 1) then
            if (associated(tab_bar_close_last_tab_choice%procedure)) then
                ! Give the handler function the full extent of the tab's
                ! data, not just its name and explicit-name flag.
                call make_tab_from_current(tab)
                call tab_bar_close_last_tab_choice%procedure(tab, status)
                call tab%destroy()
            else if (tab_bar_close_last_tab_choice%choice == TAB_BAR_CLOSE_LAST_TAB_CHOICE_DELETE_FRAME) then
                call delete_frame(status)
            else if (tab_bar_close_last_tab_choice%choice == TAB_BAR_CLOSE_LAST_TAB_CHOICE_TAB_BAR_MODE_DISABLE) then
                call minor_modes%tab_bar_mode%disable(status)
            else
                call set_error(status, FM_USER_ERROR, "Attempt to delete the sole tab in a frame")
            end if

            return
        end if

        if (close_index < 1 .or. close_index > n) return

        ! More than one tab still open.
        if (current_index == close_index) then
            ! Select another tab before deleting the current tab.
            if (present(to_number)) then
                to_index = to_number - 1
            else
                select case (tab_bar_close_tab_select%choice)
                case (TAB_BAR_CLOSE_TAB_SELECT_LEFT)
                    to_index = current_index - 2
                    if (current_index - 1 < 1) to_index = 1
                case (TAB_BAR_CLOSE_TAB_SELECT_RIGHT)
                    if (n > current_index) then
                        to_index = current_index
                    else
                        to_index = current_index - 2
                    end if
                case default
                    to_index = tab_index_recent(1) - 1
                end select
            end if

            to_index = max(0, min(to_index, n - 1))
            ! Avoid message about selected tab.
            saved = inhibit_tab_messages
            inhibit_tab_messages = .true.
            call select_tab(status, to_index + 1)
            inhibit_tab_messages = saved
            if (.not. status%ok()) return
        end if

        call push_closed_tab(close_index)
        ! Recalculate `tab-bar-lines' and update frames.
        call update_tab_bar_lines(.false.)

        if (.not. minor_modes%tab_bar_mode%enabled()) then
            call tab_message("Deleted tab and switched to "//selected_after_close_text())
        end if
    end subroutine close_tab

    !> The value of `tab_bar_close_tab_select' as a message shows it: its
    !> named constant.  Not a port.
    function selected_after_close_text() result(text)
        character(len=:), allocatable :: text

        select case (tab_bar_close_tab_select%choice)
        case (TAB_BAR_CLOSE_TAB_SELECT_LEFT)
            text = "TAB_BAR_CLOSE_TAB_SELECT_LEFT"
        case (TAB_BAR_CLOSE_TAB_SELECT_RIGHT)
            text = "TAB_BAR_CLOSE_TAB_SELECT_RIGHT"
        case default
            text = "TAB_BAR_CLOSE_TAB_SELECT_RECENT"
        end select
    end function selected_after_close_text

    !> Close the tab given its NAME.
    !> Interactively, prompt for NAME.  Port of
    !> tab-bar.el:tab-bar-close-tab-by-name.
    recursive subroutine close_tab_by_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        call update_tab_bar_tabs()
        k = tab_index_by_name(name)

        if (k == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "Wrong type argument: expected integer")
            return
        end if

        call close_tab(status, k)
    end subroutine close_tab_by_name

    !> Close all tabs on the selected frame, except the tab TAB_NUMBER.
    !> TAB_NUMBER counts from 1 and defaults to the current tab (which
    !> happens interactively).  Port of tab-bar.el:tab-bar-close-other-tabs
    !> (the hooks are Deferred).
    recursive subroutine close_other_tabs(status, tab_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number

        call close_other_tabs_keeping(status, .false., tab_number=tab_number)
    end subroutine close_other_tabs

    !> `close_other_tabs', except that with IN_GROUP only the tabs of group
    !> GROUP (unallocated for none) are closed.  Port of
    !> tab-bar.el:tab-bar-close-other-tabs (with tab-bar-close-group-tabs'
    !> prevent function when IN_GROUP).
    recursive subroutine close_other_tabs_keeping(status, in_group, group, tab_number)
        type(status_type), intent(out) :: status
        logical, intent(in) :: in_group
        character(len=:), allocatable, intent(in), optional :: group
        integer, intent(in), optional :: tab_number
        character(len=:), allocatable :: close_group, tab_group
        type(tab_type), allocatable :: remaining(:)
        integer, allocatable :: kept(:)
        logical, allocatable :: closing(:)
        integer :: n, current_index, keep_index, index, k

        status = status_type()

        if (present(group)) then
            if (allocated(group)) close_group = group
        end if

        call update_tab_bar_tabs()
        n = tab_count()
        current_index = current_tab_index()
        keep_index = current_index
        if (present(tab_number)) keep_index = max(1, min(tab_number, n))
        if (keep_index < 1 .or. keep_index > n) return

        if (keep_index /= current_index) then
            call select_tab(status, keep_index)
            if (.not. status%ok()) return
        end if

        n = tab_count()
        allocate (closing(n))
        closing = .false.

        do index = 1, n
            if (index == keep_index) cycle
            if (allocated(tab_group)) deallocate (tab_group)
            if (allocated(selected_frame%parameters%tabs(index)%group)) tab_group = selected_frame%parameters%tabs(index)%group

            if (in_group) then
                if (.not. is_same_group(tab_group, close_group)) cycle
            end if

            closing(index) = .true.
            call record_closed_tab(selected_frame%parameters%tabs(index), index)
        end do

        kept = pack([(k, k=1, n)],.not. closing)
        remaining = selected_frame%parameters%tabs(kept)
        call move_alloc(remaining, selected_frame%parameters%tabs)
        ! Recalculate tab-bar-lines and update frames.
        call update_tab_bar_lines(.false.)
        if (.not. minor_modes%tab_bar_mode%enabled()) call tab_message("Deleted all other tabs")
    end subroutine close_other_tabs_keeping

    !> Push TAB, closed at index INDEX (from 1) of the selected frame, on
    !> `tab_bar_closed_tabs' (which owns it from then on).  Not a port.
    subroutine record_closed_tab(tab, index)
        type(tab_type), intent(in) :: tab
        integer, intent(in) :: index
        type(closed_tab_type), allocatable :: grown(:)
        integer :: n

        n = 0
        if (allocated(tab_bar_closed_tabs)) n = size(tab_bar_closed_tabs)
        allocate (grown(n + 1))
        grown(1)%frame = selected_frame%handle
        grown(1)%index = index - 1
        grown(1)%tab = tab
        if (n > 0) grown(2:) = tab_bar_closed_tabs
        call move_alloc(grown, tab_bar_closed_tabs)
    end subroutine record_closed_tab

    !> Restore the most recently closed tab.  Port of
    !> tab-bar.el:tab-bar-undo-close-tab (the hook
    !> `tab_bar_post_undo_close_tab_functions' is Deferred).
    recursive subroutine undo_close_tab(status)
        type(status_type), intent(out) :: status
        type(closed_tab_type) :: closed
        type(closed_tab_type), allocatable :: rest(:)
        integer :: index

        status = status_type()

        ! Pop out closed tabs that were on already deleted frames.
        do while (allocated(tab_bar_closed_tabs))
            if (size(tab_bar_closed_tabs) == 0) exit
            if (is_frame_live(tab_bar_closed_tabs(1)%frame)) exit
            call tab_bar_closed_tabs(1)%tab%destroy()
            rest = tab_bar_closed_tabs(2:)
            call move_alloc(rest, tab_bar_closed_tabs)
        end do

        if (.not. allocated(tab_bar_closed_tabs)) then
            call tab_message("No more closed tabs to undo")
            return
        end if

        if (size(tab_bar_closed_tabs) == 0) then
            call tab_message("No more closed tabs to undo")
            return
        end if

        closed = tab_bar_closed_tabs(1)
        rest = tab_bar_closed_tabs(2:)
        call move_alloc(rest, tab_bar_closed_tabs)

        if (closed%frame%index /= selected_frame%handle%index) then
            call select_frame(closed%frame, status)
            if (.not. status%ok()) return
        end if

        call update_tab_bar_tabs()
        index = max(0, min(closed%index, tab_count()))
        call insert_tab(closed%tab, index + 1)
        call select_tab(status, index + 1)
        if (.not. status%ok()) return
        call update_tab_bar_lines(.false.)
    end subroutine undo_close_tab

    ! ------------------------------------------------------------------
    ! Renaming tabs
    ! ------------------------------------------------------------------

    !> Give the tab specified by its absolute position TAB_NUMBER a new NAME.
    !> If no TAB_NUMBER is specified, then rename the current tab.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and defaults
    !> to the current tab.
    !> TAB_NUMBER counts from 1.
    !> Interactively, prompt for the new NAME.
    !> If NAME is the empty string, then use the automatic name
    !> function `tab_bar_tab_name_function'.  Port of
    !> tab-bar.el:tab-bar-rename-tab.
    recursive subroutine rename_tab(name, status, tab_number)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number
        character(len=:), allocatable :: new_name
        integer :: k

        status = status_type()
        call update_tab_bar_tabs()

        if (present(tab_number)) then
            ! (nth -1 tabs) is the first tab.
            k = max(1, max(0, min(tab_number, tab_count())))
        else
            k = current_tab_index()
        end if

        if (k < 1) return

        if (len(name) > 0) then
            new_name = name
        else
            call tab_bar_tab_name_function(new_name)
        end if

        selected_frame%parameters%tabs(k)%name = new_name
        selected_frame%parameters%tabs(k)%explicit_name = len(name) > 0

        if (.not. minor_modes%tab_bar_mode%enabled()) then
            call tab_message(format_message("Renamed tab to '")//new_name//format_message("'"))
        end if
    end subroutine rename_tab

    !> Rename the tab named TAB_NAME to NEW_NAME.
    !> Interactively, prompt for TAB_NAME and NEW_NAME.
    !> If NEW_NAME is the empty string, then use the automatic name
    !> function `tab_bar_tab_name_function'.  Port of
    !> tab-bar.el:tab-bar-rename-tab-by-name.
    recursive subroutine rename_tab_by_name(tab_name, new_name, status)
        character(len=*), intent(in) :: tab_name, new_name
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        call update_tab_bar_tabs()
        k = tab_index_by_name(tab_name)

        if (k == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "Wrong type argument: expected integer")
            return
        end if

        call rename_tab(new_name, status, k)
    end subroutine rename_tab_by_name

    ! ------------------------------------------------------------------
    ! Tab groups
    ! ------------------------------------------------------------------

    !> Relocate tab TAB_NUMBER (by default, the current tab) closer to its
    !> group.  Port of tab-bar.el:tab-bar-move-tab-to-group (TAB given by
    !> its number).
    recursive subroutine move_tab_to_group(status, tab_number)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number
        character(len=:), allocatable :: group, other_group
        integer :: tab_index, beginning, length, pos, k, n
        logical :: has_length

        status = status_type()
        call update_tab_bar_tabs()
        n = tab_count()
        tab_index = current_tab_index()
        if (present(tab_number)) tab_index = tab_number
        if (tab_index < 1 .or. tab_index > n) return
        if (allocated(selected_frame%parameters%tabs(tab_index)%group)) group = selected_frame%parameters%tabs(tab_index)%group
        ! Beginning position of the same group (from 0).
        beginning = -1

        do k = 1, n
            if (k == tab_index) cycle
            if (allocated(other_group)) deallocate (other_group)
            if (allocated(selected_frame%parameters%tabs(k)%group)) other_group = selected_frame%parameters%tabs(k)%group

            if (is_same_group(other_group, group)) then
                beginning = k - 1
                exit
            end if
        end do

        ! Size of the same group.
        has_length = .false.
        length = 0

        if (beginning >= 0) then
            do k = beginning + 1, n
                if (allocated(other_group)) deallocate (other_group)
                if (allocated(selected_frame%parameters%tabs(k)%group)) other_group = selected_frame%parameters%tabs(k)%group

                if (.not. is_same_group(other_group, group)) then
                    has_length = .true.
                    length = k - 1 - beginning
                    exit
                end if
            end do
        end if

        ! The position to move to, from 1 (0 for none; -1 for the end).
        pos = 0

        if (beginning >= 0) then
            if (has_length .and. tab_index - 1 >= beginning .and. tab_index - 1 <= beginning + length) then
                ! Don't move tab when it's already inside group bounds.
                pos = 0
            else if (has_length .and. tab_index - 1 > beginning + length) then
                ! Move tab from the right to the group end.
                pos = beginning + length + 1
            else if (tab_index - 1 < beginning) then
                ! Move tab from the left to the group beginning.
                pos = beginning
            end if
        else
            ! Move tab with a new group to the end.
            pos = -1
        end if

        if (pos /= 0) call move_tab_to(pos, status, tab_index)
    end subroutine move_tab_to_group

    !> Add the tab specified by its absolute position TAB_NUMBER to GROUP_NAME.
    !> If no TAB_NUMBER is specified, then set the GROUP_NAME for the current tab.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and the command
    !> prompts for GROUP_NAME.
    !> TAB_NUMBER counts from 1.
    !> If GROUP_NAME is the empty string, then remove the tab from any group.
    !> While using this command, you might also want to replace
    !> `format_tab_bar_tabs' with `format_tab_bar_tabs_groups' in
    !> `tab_bar_format' to group tabs on the tab bar.
    !> Moves the tab closer to its group at the end (`move_tab_bar_tab_to_group').
    !> Port of tab-bar.el:tab-bar-change-tab-group (the hook
    !> `tab_bar_tab_post_change_group_functions' is Deferred: its default,
    !> tab-bar-move-tab-to-group, is called).
    recursive subroutine change_tab_group(group_name, status, tab_number)
        character(len=*), intent(in) :: group_name
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: tab_number
        integer :: k

        status = status_type()
        call update_tab_bar_tabs()

        if (present(tab_number)) then
            k = max(1, max(0, min(tab_number, tab_count())))
        else
            k = current_tab_index()
        end if

        if (k < 1) return

        if (len(group_name) > 0) then
            selected_frame%parameters%tabs(k)%group = group_name
        else if (allocated(selected_frame%parameters%tabs(k)%group)) then
            deallocate (selected_frame%parameters%tabs(k)%group)
        end if

        call move_tab_to_group(status, k)
        if (.not. status%ok()) return

        if (.not. minor_modes%tab_bar_mode%enabled()) then
            call tab_message(format_message("Set tab group to '")//group_name//format_message("'"))
        end if
    end subroutine change_tab_group

    !> Close all tabs that belong to GROUP_NAME on the selected frame.
    !> Interactively, prompt for GROUP_NAME.  Port of
    !> tab-bar.el:tab-bar-close-group-tabs.
    recursive subroutine close_group_tabs(group_name, status)
        character(len=*), intent(in) :: group_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: close_group, current_group
        integer :: k

        if (len(group_name) > 0) close_group = group_name
        call close_other_tabs_keeping(status, .true., close_group)
        if (.not. status%ok()) return
        k = current_tab_index()
        if (k < 1) return
        if (allocated(selected_frame%parameters%tabs(k)%group)) current_group = selected_frame%parameters%tabs(k)%group
        if (is_same_group(current_group, close_group)) call close_tab(status)
    end subroutine close_group_tabs

    ! ------------------------------------------------------------------
    ! Buffers and files in other tabs
    ! ------------------------------------------------------------------

    !> Display BUFFER in a new tab: create one with the current buffer
    !> (`tab_bar_new_tab_choice' .true.), and show BUFFER in its selected
    !> window, made for the tab.  A `display_action_function_procedure'.
    !> Port of tab-bar.el:display-buffer-in-tab (no `reusable_frames',
    !> `tab_name' or `tab_group' entries: those are Deferred) with
    !> tab-bar.el:display-buffer-in-new-tab.
    recursive subroutine display_buffer_in_tab(buffer, table, window, failed, status)
        type(handle_type), intent(in) :: buffer
        type(display_action_parameters_type), intent(in) :: table
        type(handle_type), intent(out) :: window
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status
        type(tab_bar_new_tab_choice_choice_setting_type) :: saved_choice

        failed = .false.
        window = null_handle()
        saved_choice = tab_bar_new_tab_choice
        tab_bar_new_tab_choice = .true.
        call make_tab(status)
        tab_bar_new_tab_choice = saved_choice
        if (.not. status%ok()) return
        window = window_display_buffer(buffer, selected_frame%selected_window, QR_TAB, status, table)
    end subroutine display_buffer_in_tab

    !> Switch to buffer BUFFER in another tab.
    !> Like \[switch_to_buffer_other_frame] (which see), but creates a new tab.
    !> Interactively, prompt for the buffer to switch to.  Port of
    !> tab-bar.el:switch-to-buffer-other-tab.
    recursive subroutine switch_to_buffer_other_tab(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(display_action_parameters_type) :: table

        table%inhibit_same_window = .false.
        call pop_to_buffer(buffer, status, table=table, action_function=display_buffer_in_tab)
    end subroutine switch_to_buffer_other_tab

    !> Edit file FILENAME, in another tab.
    !> Like \[find_file_other_frame] (which see), but creates a new tab.
    !> Interactively, prompt for FILENAME.  Port of
    !> tab-bar.el:find-file-other-tab (WILDCARDS is Deferred).
    recursive subroutine find_file_other_tab(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call find_file_noselect(filename, buffer, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_tab(buffer, status)
    end subroutine find_file_other_tab

    !> Edit file FILENAME, in another tab, but don't allow changes.
    !> Like \[find_file_other_frame] (which see), but creates a new tab.
    !> Like \[find_file_other_tab], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.
    !> Interactively, prompt for FILENAME.  Port of
    !> tab-bar.el:find-file-read-only-other-tab, with
    !> files.el:find-file--read-only (WILDCARDS is Deferred).
    recursive subroutine find_file_read_only_other_tab(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. file_exists(filename)) then
            call set_error(status, FM_GENERIC_ERROR, filename//" does not exist")
            return
        end if

        call find_file_other_tab(filename, status)
        if (.not. status%ok()) return
        call minor_modes%read_only_mode%enable(status)
    end subroutine find_file_read_only_other_tab

    ! ------------------------------------------------------------------
    ! The tab bar of a frame
    ! ------------------------------------------------------------------

    !> Toggle tab bar of the selected frame.
    !> This is useful if you want to enable the tab bar individually
    !> on each new frame when the global `tab_bar_mode' is disabled,
    !> or if you want to disable the tab bar individually on each
    !> new frame when the global `tab_bar_mode' is enabled.  Port of
    !> tab-bar.el:toggle-frame-tab-bar (FRAME the selected frame).
    subroutine toggle_frame_tab_bar(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call ensure_frame_windows()

        if (selected_frame%tab_bar_lines > 0) then
            call set_tab_bar_lines(selected_frame, 0)
        else
            call set_tab_bar_lines(selected_frame, 1)
        end if

        selected_frame%parameters%tab_bar_lines_keep_state = .not. selected_frame%parameters%tab_bar_lines_keep_state
    end subroutine toggle_frame_tab_bar

    !> Close the current tab when the window deleted is its last one, and
    !> say so (`close_tab_hook').  Port of window.el:window--delete (its
    !> `tab' case).
    recursive subroutine close_tab_after_last_window(status)
        type(status_type), intent(out) :: status

        call close_tab(status)
        if (.not. status%ok()) return
        call message("Tab closed after deleting the last window")
    end subroutine close_tab_after_last_window

    ! ------------------------------------------------------------------
    ! Tab history mode
    ! ------------------------------------------------------------------

    !> Toggle tab history mode for the tab bar.
    !> Tab history mode remembers window configurations used in every tab,
    !> and can restore them.  Port of tab-bar.el:tab-bar-history-mode.
    recursive subroutine tab_bar_history_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%tab_bar_history_mode%command(status)
    end subroutine tab_bar_history_mode

    !> The body of tab history mode.  Port of
    !> tab-bar.el:tab-bar-history-mode (its body; the images of the buttons
    !> wait for the GUI).
    recursive subroutine tab_bar_history_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            tab_bar_back_button = " < "
            tab_bar_forward_button = " > "
            call pre_command_hook%prepend(tab_bar_history_pre_change)
            call window_configuration_change_hook%prepend(tab_bar_history_change)
        else
            call pre_command_hook%remove(tab_bar_history_pre_change)
            call window_configuration_change_hook%remove(tab_bar_history_change)
        end if
    end subroutine tab_bar_history_mode_body

    !> Before a command: forget `tab_bar_history_omit', remember the command
    !> and, outside the minibuffer, the window configuration.  Port of
    !> tab-bar.el:tab-bar--history-pre-change.
    subroutine tab_bar_history_pre_change(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ! Reset before the command could set it.
        tab_bar_history_omit = .false.
        tab_bar_history_pre_command => this_command

        if (minibuf_level == 0) then
            if (has_tab_bar_history_old) call tab_bar_history_old%destroy()
            call current_tab_window_configuration(tab_bar_history_old)
            has_tab_bar_history_old = .true.
        end if
    end subroutine tab_bar_history_pre_change

    !> Whether commands A and B are the same (both none, or the same).
    !> Not a port (`eq').
    logical function is_same_command(a, b)
        type(command_type), pointer, intent(in) :: a, b

        if (.not. associated(a)) then
            is_same_command = .not. associated(b)
        else
            is_same_command = associated(a, b)
        end if
    end function is_same_command

    !> After a change of the window configuration: put the configuration
    !> before the command first in the frame's back history (keeping
    !> `tab_bar_history_limit'), unless the same command changed it just
    !> before.  Port of tab-bar.el:tab-bar--history-change.
    subroutine tab_bar_history_change(status)
        type(status_type), intent(out) :: status
        type(tab_window_configuration_type), allocatable :: history(:)
        integer :: n, k

        status = status_type()

        if (.not. tab_bar_history_omit .and. has_tab_bar_history_old .and. &
            .not. is_same_command(tab_bar_history_done_command, tab_bar_history_pre_command) .and. minibuf_level == 0) then
            n = 0
            if (allocated(selected_frame%tab_history_back)) n = size(selected_frame%tab_history_back)
            allocate (history(min(n + 1, max(tab_bar_history_limit, 0))))

            if (size(history) > 0) then
                history(1) = tab_bar_history_old

                do k = 2, size(history)
                    history(k) = selected_frame%tab_history_back(k - 1)
                end do

                ! Free the configurations that fall off the end.
                do k = size(history), n
                    call selected_frame%tab_history_back(k)%destroy()
                end do
            else
                call tab_bar_history_old%destroy()
            end if

            call move_alloc(history, selected_frame%tab_history_back)
            has_tab_bar_history_old = .false.
        end if

        tab_bar_history_done_command => tab_bar_history_pre_command
    end subroutine tab_bar_history_change

    !> Pop the first window configuration of HISTORY (in CONFIG; FOUND
    !> false when HISTORY is empty).  Not a port.
    subroutine pop_history(history, config, found)
        type(tab_window_configuration_type), allocatable, intent(inout) :: history(:)
        type(tab_window_configuration_type), intent(out) :: config
        logical, intent(out) :: found
        type(tab_window_configuration_type), allocatable :: rest(:)

        found = .false.
        if (.not. allocated(history)) return
        if (size(history) == 0) return
        config = history(1)
        rest = history(2:)
        call move_alloc(rest, history)
        found = .true.
    end subroutine pop_history

    !> Put a copy of `tab_bar_history_old' (or no configuration, while
    !> there is none) first in HISTORY.  Not a port.
    subroutine push_history_old(history)
        type(tab_window_configuration_type), allocatable, intent(inout) :: history(:)
        type(tab_window_configuration_type), allocatable :: grown(:)
        integer :: n

        n = 0
        if (allocated(history)) n = size(history)
        allocate (grown(n + 1))
        if (has_tab_bar_history_old) call copy_tab_window_configuration(tab_bar_history_old, grown(1))
        if (n > 0) grown(2:) = history
        call move_alloc(grown, history)
    end subroutine push_history_old

    !> Restore window configuration CONFIG (if it is one, else say TEXT)
    !> after putting `tab_bar_history_old' first in OTHER.  Port of the body
    !> of tab-bar.el:tab-bar-history-back and tab-bar-history-forward.
    subroutine restore_tab_history(config, found, other, text, status)
        type(tab_window_configuration_type), intent(inout) :: config
        logical, intent(in) :: found
        type(tab_window_configuration_type), allocatable, intent(inout) :: other(:)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: b
        integer(int64) :: pos

        status = status_type()

        if (.not. found .or. .not. allocated(config%window_configuration%saved)) then
            if (found) call config%destroy()
            call message(text)
            return
        end if

        call push_history_old(other)
        call set_window_configuration(config%window_configuration)

        if (config%point%index /= 0) then
            call marker_buffer(config%point, b, status)

            if (status%ok() .and. is_buffer_live(b)) then
                call marker_position(config%point, pos, status)
                if (status%ok()) call goto_char(pos)
            end if

            status = status_type()
        end if

        call config%destroy()
    end subroutine restore_tab_history

    !> Restore a previous window configuration used in the current tab.
    !> This navigates back in the history of window configurations.
    !> Port of tab-bar.el:tab-bar-history-back.
    recursive subroutine go_back_in_tab_bar_history(status)
        type(status_type), intent(out) :: status
        type(tab_window_configuration_type) :: config
        logical :: found

        tab_bar_history_omit = .true.
        call pop_history(selected_frame%tab_history_back, config, found)
        call restore_tab_history(config, found, selected_frame%tab_history_forward, "No more tab back history", status)
    end subroutine go_back_in_tab_bar_history

    !> Cancel restoration of the previous window configuration.
    !> This navigates forward in the history of window configurations.
    !> Port of tab-bar.el:tab-bar-history-forward.
    recursive subroutine go_forward_in_tab_bar_history(status)
        type(status_type), intent(out) :: status
        type(tab_window_configuration_type) :: config
        logical :: found

        tab_bar_history_omit = .true.
        call pop_history(selected_frame%tab_history_forward, config, found)
        call restore_tab_history(config, found, selected_frame%tab_history_back, "No more tab forward history", status)
    end subroutine go_forward_in_tab_bar_history

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The prefix argument as a number when it is one (upstream's
    !> `integerp' of the raw prefix argument).  Not a port.
    logical function has_number_prefix()
        has_number_prefix = current_prefix_arg%kind == PREFIX_NUMBER
    end function has_number_prefix

    !> The names of the selected frame's tabs (the most recently visited
    !> first, other than the current one, with RECENT) as completion
    !> candidates.  Not a port.
    function tab_names(recent) result(names)
        logical, intent(in) :: recent
        type(value_type), allocatable :: names(:)
        integer, allocatable :: order(:)
        integer :: k

        call update_tab_bar_tabs()

        if (recent) then
            order = tabs_recent()
        else
            order = [(k, k=1, tab_count())]
        end if

        allocate (names(size(order)))

        do k = 1, size(order)
            names(k) = value_type(text=selected_frame%parameters%tabs(order(k))%name)
        end do
    end function tab_names

    !> The groups of the selected frame's tabs, each once, the group of tab
    !> FIRST (if any) first, as completion candidates.  Not a port.
    function group_names(first) result(names)
        integer, intent(in) :: first
        type(value_type), allocatable :: names(:)
        type(string_type), allocatable :: groups(:)
        integer :: k

        allocate (groups(0))
        if (first >= 1 .and. first <= tab_count()) call add_group(first)

        do k = 1, tab_count()
            call add_group(k)
        end do

        allocate (names(size(groups)))

        do k = 1, size(groups)
            names(k) = value_type(text=groups(k)%text)
        end do

    contains

        !> Add the group of tab J if it has one not added yet.
        subroutine add_group(j)
            integer, intent(in) :: j
            integer :: i

            if (.not. allocated(selected_frame%parameters%tabs(j)%group)) return

            do i = 1, size(groups)
                if (groups(i)%text == selected_frame%parameters%tabs(j)%group .and. &
                    len(groups(i)%text) == len(selected_frame%parameters%tabs(j)%group)) return
            end do

            call append_string(groups, selected_frame%parameters%tabs(j)%group)
        end subroutine add_group
    end function group_names

    !> Switch to the tab by its absolute position TAB_NUMBER in the tab bar.
    !> When this command is bound to a numeric key (with a key prefix or modifier key
    !> using `tab_bar_select_tab_modifiers'), calling it without an argument
    !> will translate its bound numeric key to the numeric argument.
    !> Also the prefix argument TAB_NUMBER can be used to override
    !> the numeric key, so it takes precedence over the bound digit key.
    !> For example, `<MODIFIER>-2' will select the second tab, but `C-u 15
    !> <MODIFIER>-2' will select the 15th tab.  TAB_NUMBER counts from 1.
    !> Negative TAB_NUMBER counts tabs from the end of the tab bar.
    !> Port of tab-bar.el:tab-bar-select-tab (the command; tab-select).
    recursive subroutine cmd_select_tab(status)
        type(status_type), intent(out) :: status

        if (has_number_prefix()) then
            call select_tab(status, int(current_prefix_arg%value))
        else
            call select_tab(status)
        end if
    end subroutine cmd_select_tab

    !> Switch to ARGth next tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-next-tab (the command; tab-next).
    recursive subroutine cmd_switch_to_next_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_next_tab(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_switch_to_next_tab

    !> Switch to ARGth previous tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-prev-tab (the command;
    !> tab-previous).
    recursive subroutine cmd_switch_to_previous_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_previous_tab(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_switch_to_previous_tab

    !> Switch to the last tab or ARGth tab from the end of the tab bar.
    !> Interactively, ARG is the prefix numeric argument; it defaults to 1,
    !> which means the last tab on the tab bar.  For example, `C-u 2
    !> <MODIFIER>-9' selects the tab before the last tab.  Port of
    !> tab-bar.el:tab-bar-switch-to-last-tab (the command; tab-last).
    recursive subroutine cmd_switch_to_last_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_last_tab(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_switch_to_last_tab

    !> Switch to ARGth most recently visited tab.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Port of tab-bar.el:tab-bar-switch-to-recent-tab (the command;
    !> tab-recent).
    recursive subroutine cmd_switch_to_recent_tab(status)
        type(status_type), intent(out) :: status

        call switch_to_recent_tab(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_switch_to_recent_tab

    !> Switch to the tab by NAME.
    !> Default values are tab names sorted by recency, so you can use
    !> \<minibuffer_local_map>\[next_history_element]
    !> to get the name of the most recently visited tab, the second
    !> most recent, and so on.
    !> When the tab with that NAME doesn't exist, create a new tab
    !> and rename it to NAME.  Port of tab-bar.el:tab-bar-switch-to-tab (the
    !> command; tab-switch).
    recursive subroutine cmd_switch_to_tab(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        type(string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: name, prompt
        integer, allocatable :: order(:)
        integer :: k

        names = tab_names(.true.)
        order = tabs_recent()
        allocate (defaults(size(order)))

        do k = 1, size(order)
            defaults(k)%text = selected_frame%parameters%tabs(order(k))%name
        end do

        prompt = "Switch to tab by name: "
        if (size(defaults) > 0) prompt = "Switch to tab by name (default "//defaults(1)%text//"): "
        call completing_read(prompt, completion_list_table(make_list(names)), name, status, defaults=defaults)
        if (.not. status%ok()) return
        call switch_to_tab(name, status)
    end subroutine cmd_switch_to_tab

    !> Move tab from FROM_NUMBER position to new position at TO_NUMBER.
    !> FROM_NUMBER defaults to the current tab number.
    !> FROM_NUMBER and TO_NUMBER count from 1.
    !> Negative TO_NUMBER counts tabs from the end of the tab bar.
    !> Argument addressing is absolute in contrast to `move_tab'
    !> where argument addressing is relative.  Port of
    !> tab-bar.el:tab-bar-move-tab-to (the command; tab-move-to).
    subroutine cmd_move_tab_to(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call move_tab_to(1, status)
        else
            call move_tab_to(int(prefix_numeric_value(current_prefix_arg)), status)
        end if
    end subroutine cmd_move_tab_to

    !> Move the current tab ARG positions to the right.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> If ARG is negative, move the current tab ARG positions to the left.
    !> Argument addressing is relative in contrast to `move_tab_to',
    !> where argument addressing is absolute.  Port of
    !> tab-bar.el:tab-bar-move-tab (the command; tab-move).
    subroutine cmd_move_tab(status)
        type(status_type), intent(out) :: status

        call move_tab(int(prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_move_tab

    !> Move the current tab ARG positions to the left.
    !> Interactively, ARG is the prefix numeric argument and defaults to 1.
    !> Like `move_tab', but moves in the opposite direction.  Port of
    !> tab-bar.el:tab-bar-move-tab-backward (the command).
    subroutine cmd_move_tab_backward(status)
        type(status_type), intent(out) :: status

        call move_tab_backward(int(prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_move_tab_backward

    !> Add a new tab at the absolute position TAB_NUMBER.
    !> TAB_NUMBER counts from 1.  If no TAB_NUMBER is specified, then add
    !> a new tab at the position specified by `tab_bar_new_tab_to'.
    !> Negative TAB_NUMBER counts tabs from the end of the tab bar,
    !> and -1 means the new tab will become the last one.
    !> Argument addressing is absolute in contrast to `make_tab',
    !> where argument addressing is relative.  Port of
    !> tab-bar.el:tab-bar-new-tab-to (the command; tab-new-to).
    recursive subroutine cmd_make_tab_to(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call make_tab_to(status)
        else
            call make_tab_to(status, int(prefix_numeric_value(current_prefix_arg)))
        end if
    end subroutine cmd_make_tab_to

    !> Create a new tab ARG positions to the right.
    !> If a negative ARG, create a new tab ARG positions to the left.
    !> If ARG is zero, create a new tab in place of the current tab.
    !> If no ARG is specified, then add a new tab at the position
    !> specified by `tab_bar_new_tab_to'.
    !> Argument addressing is relative in contrast to `make_tab_to',
    !> where argument addressing is absolute.  Port of
    !> tab-bar.el:tab-bar-new-tab (the command; tab-new).
    recursive subroutine cmd_make_tab(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call make_tab(status)
        else
            call make_tab(status, int(prefix_numeric_value(current_prefix_arg)))
        end if
    end subroutine cmd_make_tab

    !> Clone the current tab to ARG positions to the right.
    !> ARG has the same meaning as in `make_tab'.  Port of
    !> tab-bar.el:tab-bar-duplicate-tab (the command; tab-duplicate).
    recursive subroutine cmd_duplicate_tab(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call duplicate_tab(status)
        else
            call duplicate_tab(status, int(prefix_numeric_value(current_prefix_arg)))
        end if
    end subroutine cmd_duplicate_tab

    !> Close the tab specified by its absolute position TAB_NUMBER.
    !> If no TAB_NUMBER is specified, then close the current tab and switch
    !> to the tab specified by `tab_bar_close_tab_select'.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and defaults to 1.
    !> TAB_NUMBER counts from 1.
    !>
    !> The base behavior for the last tab on a frame is determined by
    !> `tab_bar_close_last_tab_choice'.  Port of tab-bar.el:tab-bar-close-tab
    !> (the command; tab-close).
    recursive subroutine cmd_close_tab(status)
        type(status_type), intent(out) :: status

        if (has_number_prefix()) then
            call close_tab(status, int(current_prefix_arg%value))
        else
            call close_tab(status)
        end if
    end subroutine cmd_close_tab

    !> Close the tab given its NAME.
    !> Interactively, prompt for NAME.  Port of
    !> tab-bar.el:tab-bar-close-tab-by-name (the command).
    recursive subroutine cmd_close_tab_by_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call completing_read("Close tab by name: ", completion_list_table(make_list(tab_names(.false.))), name, status)
        if (.not. status%ok()) return
        call close_tab_by_name(name, status)
    end subroutine cmd_close_tab_by_name

    !> Close all tabs on the selected frame, except the current tab.
    !> Port of tab-bar.el:tab-bar-close-other-tabs (the command;
    !> tab-close-other).
    recursive subroutine cmd_close_other_tabs(status)
        type(status_type), intent(out) :: status

        call close_other_tabs(status)
    end subroutine cmd_close_other_tabs

    !> Restore the most recently closed tab.  Port of
    !> tab-bar.el:tab-bar-undo-close-tab (the command; tab-undo).
    recursive subroutine cmd_undo_close_tab(status)
        type(status_type), intent(out) :: status

        call undo_close_tab(status)
    end subroutine cmd_undo_close_tab

    !> Give the tab specified by its absolute position TAB_NUMBER a new NAME.
    !> If no TAB_NUMBER is specified, then rename the current tab.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and defaults
    !> to the current tab.
    !> TAB_NUMBER counts from 1.
    !> Interactively, prompt for the new NAME.
    !> If NAME is the empty string, then use the automatic name
    !> function `tab_bar_tab_name_function'.  Port of
    !> tab-bar.el:tab-bar-rename-tab (the command; tab-rename).
    recursive subroutine cmd_rename_tab(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        integer :: k

        call update_tab_bar_tabs()
        k = current_tab_index()
        if (current_prefix_arg%kind /= PREFIX_NONE) k = int(prefix_numeric_value(current_prefix_arg))
        k = max(1, min(k, tab_count()))
        call read_from_minibuffer("New name for tab (leave blank for automatic naming): ", name, status, &
                                  default=selected_frame%parameters%tabs(k)%name)
        if (.not. status%ok()) return

        if (has_number_prefix()) then
            call rename_tab(name, status, int(current_prefix_arg%value))
        else
            call rename_tab(name, status)
        end if
    end subroutine cmd_rename_tab

    !> Rename the tab named TAB_NAME to NEW_NAME.
    !> Interactively, prompt for TAB_NAME and NEW_NAME.
    !> If NEW_NAME is the empty string, then use the automatic name
    !> function `tab_bar_tab_name_function'.  Port of
    !> tab-bar.el:tab-bar-rename-tab-by-name (the command).
    recursive subroutine cmd_rename_tab_by_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tab_name, new_name

        call completing_read("Rename tab by name: ", completion_list_table(make_list(tab_names(.false.))), tab_name, &
                             status)
        if (.not. status%ok()) return
        call read_from_minibuffer("New name for tab (leave blank for automatic naming): ", new_name, status, &
                                  default=tab_name)
        if (.not. status%ok()) return
        call rename_tab_by_name(tab_name, new_name, status)
    end subroutine cmd_rename_tab_by_name

    !> Relocate the current tab closer to its group.  Port of
    !> tab-bar.el:tab-bar-move-tab-to-group (the command).
    recursive subroutine cmd_move_tab_to_group(status)
        type(status_type), intent(out) :: status

        call move_tab_to_group(status)
    end subroutine cmd_move_tab_to_group

    !> Add the tab specified by its absolute position TAB_NUMBER to GROUP_NAME.
    !> If no TAB_NUMBER is specified, then set the GROUP_NAME for the current tab.
    !> Interactively, TAB_NUMBER is the prefix numeric argument, and the command
    !> prompts for GROUP_NAME.
    !> TAB_NUMBER counts from 1.
    !> If GROUP_NAME is the empty string, then remove the tab from any group.
    !> While using this command, you might also want to replace
    !> `format_tab_bar_tabs' with `format_tab_bar_tabs_groups' in
    !> `tab_bar_format' to group tabs on the tab bar.  Port of
    !> tab-bar.el:tab-bar-change-tab-group (the command; tab-group).
    recursive subroutine cmd_change_tab_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: group
        integer :: k

        call update_tab_bar_tabs()
        k = current_tab_index()
        if (current_prefix_arg%kind /= PREFIX_NONE) k = int(prefix_numeric_value(current_prefix_arg))
        call completing_read("Group name for tab (leave blank to remove group): ", &
                             completion_list_table(make_list(group_names(k))), group, status)
        if (.not. status%ok()) return

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call change_tab_group(group, status, int(prefix_numeric_value(current_prefix_arg)))
        else
            call change_tab_group(group, status)
        end if
    end subroutine cmd_change_tab_group

    !> Close all tabs that belong to GROUP_NAME on the selected frame.
    !> Interactively, prompt for GROUP_NAME.  Port of
    !> tab-bar.el:tab-bar-close-group-tabs (the command; tab-close-group).
    recursive subroutine cmd_close_group_tabs(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: group

        call update_tab_bar_tabs()
        call completing_read("Close all tabs with group name: ", &
                             completion_list_table(make_list(group_names(current_tab_index()))), group, status)
        if (.not. status%ok()) return
        call close_group_tabs(group, status)
    end subroutine cmd_close_group_tabs

    !> Switch to buffer BUFFER_OR_NAME in another tab.
    !> Like \[switch_to_buffer_other_frame] (which see), but creates a new tab.
    !> Interactively, prompt for the buffer to switch to.  Port of
    !> tab-bar.el:switch-to-buffer-other-tab (the command, with
    !> window.el:read-buffer-to-switch and
    !> window.el:window-normalize-buffer-to-switch-to).
    recursive subroutine cmd_switch_to_buffer_other_tab(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: other, buffer
        integer(int64) :: bi

        other = other_buffer(current_buffer())
        call resolve_buffer(other, bi, status)
        if (.not. status%ok()) return
        call read_buffer("Switch to buffer in other tab: ", name, status, def=buffers(bi)%name, &
                         require_match=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, except=current_buffer())
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(bi)%name
        buffer = get_buffer(name)

        if (.not. is_buffer_live(buffer)) then
            call get_or_make_buffer(name, buffer, status)
            if (.not. status%ok()) return
        end if

        call switch_to_buffer_other_tab(buffer, status)
    end subroutine cmd_switch_to_buffer_other_tab

    !> Edit file FILENAME, in another tab.
    !> Like \[find_file_other_frame] (which see), but creates a new tab.
    !> Interactively, prompt for FILENAME.  Port of
    !> tab-bar.el:find-file-other-tab (the command, with
    !> files.el:find-file-read-args).
    recursive subroutine cmd_find_file_other_tab(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir

        dir = "~/"
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
        call read_file_name("Find file in other tab: ", filename, status, default_filename=dir, &
                            mustmatch=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION)
        if (.not. status%ok()) return
        call find_file_other_tab(filename, status)
    end subroutine cmd_find_file_other_tab

    !> Edit file FILENAME, in another tab, but don't allow changes.
    !> Like \[find_file_other_frame] (which see), but creates a new tab.
    !> Like \[find_file_other_tab], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.
    !> Interactively, prompt for FILENAME.  Port of
    !> tab-bar.el:find-file-read-only-other-tab (the command, with
    !> files.el:find-file-read-args).
    recursive subroutine cmd_find_file_read_only_other_tab(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir

        dir = "~/"
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
        call read_file_name("Find file read-only in other tab: ", filename, status, default_filename=dir, &
                            mustmatch=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION)
        if (.not. status%ok()) return
        call find_file_read_only_other_tab(filename, status)
    end subroutine cmd_find_file_read_only_other_tab

    !> Toggle tab bar of the selected frame.
    !> This is useful if you want to enable the tab bar individually
    !> on each new frame when the global `tab_bar_mode' is disabled,
    !> or if you want to disable the tab bar individually on each
    !> new frame when the global `tab_bar_mode' is enabled.  Port of
    !> tab-bar.el:toggle-frame-tab-bar (the command).
    subroutine cmd_toggle_frame_tab_bar(status)
        type(status_type), intent(out) :: status

        call toggle_frame_tab_bar(status)
    end subroutine cmd_toggle_frame_tab_bar

    !> Toggle tab bar on or off, based on the status of the current frame.
    !> Used in the Show/Hide menu, to have the toggle reflect the current frame.
    !> See `tab_bar_mode' for more information.  Port of
    !> tab-bar.el:toggle-tab-bar-mode-from-frame (the command).
    recursive subroutine cmd_toggle_tab_bar_mode_from_frame(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call minor_modes%tab_bar_mode%command(status)
        else if (selected_frame%tab_bar_lines > 0) then
            call minor_modes%tab_bar_mode%disable(status)
        else
            call minor_modes%tab_bar_mode%enable(status)
        end if
    end subroutine cmd_toggle_tab_bar_mode_from_frame

    !> Move the selected window to a new tab.
    !> This command removes the selected window from the configuration stored
    !> on the current tab, and makes a new tab with that window in its
    !> configuration.  Port of tab-bar.el:tab-bar-move-window-to-tab (the
    !> command; tab-window-detach).
    recursive subroutine cmd_move_window_to_tab(status)
        type(status_type), intent(out) :: status

        call move_window_to_tab(status)
    end subroutine cmd_move_window_to_tab

    !> Restore a previous window configuration used in the current tab.
    !> This navigates back in the history of window configurations.
    !> Port of tab-bar.el:tab-bar-history-back (the command).
    recursive subroutine cmd_go_back_in_tab_bar_history(status)
        type(status_type), intent(out) :: status

        call go_back_in_tab_bar_history(status)
    end subroutine cmd_go_back_in_tab_bar_history

    !> Cancel restoration of the previous window configuration.
    !> This navigates forward in the history of window configurations.
    !> Port of tab-bar.el:tab-bar-history-forward (the command).
    recursive subroutine cmd_go_forward_in_tab_bar_history(status)
        type(status_type), intent(out) :: status

        call go_forward_in_tab_bar_history(status)
    end subroutine cmd_go_forward_in_tab_bar_history

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define the tab bar's faces, modes, commands and keymaps, and bind
    !> the tab commands in TAB_PREFIX_MAP (C-x t), as loading tab-bar.el
    !> does.  Not a port.
    subroutine init_tab_bar(tab_prefix_map)
        type(handle_type), intent(in) :: tab_prefix_map
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK
        type(face_attributes_type) :: attrs
        type(status_type) :: st

        call tab_bar_tab%define("tab_bar_tab", "Tab bar face for selected tab.")
        call tab_bar_tab%spec_entry(display_when(is_default=.true.), face_attrs(inherit="tab_bar"))
        attrs = face_attrs()
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_bar_tab%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), attrs)
        attrs = face_attrs(background="grey40", foreground="white")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_bar_tab%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), attrs)
        call tab_bar_tab%spec_entry(display_when(), face_attrs(inverse_video=.false.))
        call tab_bar_tab_inactive%define("tab_bar_tab_inactive", "Tab bar face for non-selected tab.")
        call tab_bar_tab_inactive%spec_entry(display_when(is_default=.true.), face_attrs(inherit="tab_bar_tab"))
        call tab_bar_tab_inactive%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                             face_attrs(background="grey75"))
        call tab_bar_tab_inactive%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                             face_attrs(background="grey20"))
        call tab_bar_tab_inactive%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call tab_bar_tab_group_current%define("tab_bar_tab_group_current", "Tab bar face for current group tab.")
        call tab_bar_tab_group_current%spec_entry(display_when(), face_attrs(inherit="tab_bar_tab", box=.false., &
                                                                             weight="bold"))
        call tab_bar_tab_group_inactive%define("tab_bar_tab_group_inactive", "Tab bar face for inactive group tab.")
        call tab_bar_tab_group_inactive%spec_entry(display_when(), &
                                                   face_attrs(inherits=[character(len=20) :: "shadow", &
                                                                        "tab_bar_tab_inactive"]))
        call tab_bar_tab_ungrouped%define("tab_bar_tab_ungrouped", &
                                          "Tab bar face for ungrouped tab when tab groups are used.")
        call tab_bar_tab_ungrouped%spec_entry(display_when(), &
                                              face_attrs(inherits=[character(len=20) :: "shadow", "tab_bar_tab_inactive"]))
        call tab_bar_tab_highlight%define("tab_bar_tab_highlight", "Tab bar face for highlighting.")
        attrs = face_attrs(background="grey85", foreground="black")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_bar_tab_highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), attrs)
        attrs = face_attrs(background="grey40", foreground="white")
        attrs%box = face_box(line_width=1, style=FACE_BOX_RELEASED_BUTTON)
        call tab_bar_tab_highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), attrs)
        call tab_bar_tab_highlight%spec_entry(display_when(), face_attrs(inverse_video=.false.))
        if (.not. allocated(tab_bar_new_button)) tab_bar_new_button = " + "
        if (.not. allocated(tab_bar_close_button)) tab_bar_close_button = " x"
        if (.not. allocated(tab_bar_back_button)) tab_bar_back_button = " < "
        if (.not. allocated(tab_bar_forward_button)) tab_bar_forward_button = " > "

        if (.not. allocated(tab_bar_format%entries)) then
            call tab_bar_format%append(format_tab_bar_history)
            call tab_bar_format%append(format_tab_bar_tabs)
            call tab_bar_format%append(format_tab_bar_separator)
            call tab_bar_format%append(format_tab_bar_add_tab)
        end if

        call minor_modes%tab_bar_mode%define("tab_bar_mode", global=.true., hook=tab_bar_mode_hook, keymap=.true., &
                                             body=tab_bar_mode_body)
        tab_bar_mode_map = minor_modes%tab_bar_mode%map
        call minor_modes%tab_bar_history_mode%define("tab_bar_history_mode", global=.true., &
                                                     hook=tab_bar_history_mode_hook, keymap=.true., &
                                                     body=tab_bar_history_mode_body)
        tab_bar_history_mode_map = minor_modes%tab_bar_history_mode%map
        call keymap_set(tab_bar_history_mode_map, "C-c <left>", "go_back_in_tab_bar_history", st)
        call keymap_set(tab_bar_history_mode_map, "C-c <right>", "go_forward_in_tab_bar_history", st)
        commands%tab_bar_mode%execute => tab_bar_mode
        commands%tab_bar_history_mode%execute => tab_bar_history_mode
        commands%select_tab_bar_tab%execute => cmd_select_tab
        commands%switch_to_tab_bar_next_tab%execute => cmd_switch_to_next_tab
        commands%switch_to_tab_bar_previous_tab%execute => cmd_switch_to_previous_tab
        commands%switch_to_tab_bar_last_tab%execute => cmd_switch_to_last_tab
        commands%switch_to_tab_bar_recent_tab%execute => cmd_switch_to_recent_tab
        commands%switch_to_tab_bar_tab%execute => cmd_switch_to_tab
        commands%move_tab_bar_tab_to%execute => cmd_move_tab_to
        commands%move_tab_bar_tab%execute => cmd_move_tab
        commands%move_tab_bar_tab_backward%execute => cmd_move_tab_backward
        commands%make_tab_bar_tab_to%execute => cmd_make_tab_to
        commands%make_tab_bar_tab%execute => cmd_make_tab
        commands%duplicate_tab_bar_tab%execute => cmd_duplicate_tab
        commands%close_tab_bar_tab%execute => cmd_close_tab
        commands%close_tab_bar_tab_by_name%execute => cmd_close_tab_by_name
        commands%close_tab_bar_other_tabs%execute => cmd_close_other_tabs
        commands%undo_close_tab_bar_tab%execute => cmd_undo_close_tab
        commands%rename_tab_bar_tab%execute => cmd_rename_tab
        commands%rename_tab_bar_tab_by_name%execute => cmd_rename_tab_by_name
        commands%move_tab_bar_tab_to_group%execute => cmd_move_tab_to_group
        commands%change_tab_bar_tab_group%execute => cmd_change_tab_group
        commands%close_tab_bar_group_tabs%execute => cmd_close_group_tabs
        commands%switch_to_buffer_other_tab%execute => cmd_switch_to_buffer_other_tab
        commands%find_file_other_tab%execute => cmd_find_file_other_tab
        commands%find_file_read_only_other_tab%execute => cmd_find_file_read_only_other_tab
        commands%toggle_frame_tab_bar%execute => cmd_toggle_frame_tab_bar
        commands%toggle_tab_bar_mode_from_frame%execute => cmd_toggle_tab_bar_mode_from_frame
        commands%move_tab_bar_window_to_tab%execute => cmd_move_window_to_tab
        commands%go_back_in_tab_bar_history%execute => cmd_go_back_in_tab_bar_history
        commands%go_forward_in_tab_bar_history%execute => cmd_go_forward_in_tab_bar_history
        ! The aliases (tab-bar.el's defalias of tab-new and the others, and
        ! of tab-bar-select-tab-by-name).
        commands%select_tab_bar_tab_by_name%execute => cmd_switch_to_tab
        commands%make_tab%execute => cmd_make_tab
        commands%make_tab_to%execute => cmd_make_tab_to
        commands%duplicate_tab%execute => cmd_duplicate_tab
        commands%detach_tab_window%execute => cmd_move_window_to_tab
        commands%close_tab%execute => cmd_close_tab
        commands%close_other_tabs%execute => cmd_close_other_tabs
        commands%close_tab_group%execute => cmd_close_group_tabs
        commands%undo_tab%execute => cmd_undo_close_tab
        commands%select_tab%execute => cmd_select_tab
        commands%switch_tab%execute => cmd_switch_to_tab
        commands%next_tab%execute => cmd_switch_to_next_tab
        commands%previous_tab%execute => cmd_switch_to_previous_tab
        commands%last_tab%execute => cmd_switch_to_last_tab
        commands%recent_tab%execute => cmd_switch_to_recent_tab
        commands%move_tab%execute => cmd_move_tab
        commands%move_tab_to%execute => cmd_move_tab_to
        commands%rename_tab%execute => cmd_rename_tab
        commands%change_tab_group%execute => cmd_change_tab_group
        ! The short aliases' keys (tab-bar.el's keymap-set of tab-prefix-map).
        call keymap_set(tab_prefix_map, "n", "duplicate_tab", st)
        call keymap_set(tab_prefix_map, "N", "make_tab_to", st)
        call keymap_set(tab_prefix_map, "2", "make_tab", st)
        call keymap_set(tab_prefix_map, "1", "close_other_tabs", st)
        call keymap_set(tab_prefix_map, "0", "close_tab", st)
        call keymap_set(tab_prefix_map, "u", "undo_tab", st)
        call keymap_set(tab_prefix_map, "o", "next_tab", st)
        call keymap_set(tab_prefix_map, "O", "previous_tab", st)
        call keymap_set(tab_prefix_map, "m", "move_tab", st)
        call keymap_set(tab_prefix_map, "M", "move_tab_to", st)
        call keymap_set(tab_prefix_map, "G", "change_tab_group", st)
        call keymap_set(tab_prefix_map, "r", "rename_tab", st)
        call keymap_set(tab_prefix_map, "RET", "switch_tab", st)
        call keymap_set(tab_prefix_map, "b", "switch_to_buffer_other_tab", st)
        call keymap_set(tab_prefix_map, "f", "find_file_other_tab", st)
        call keymap_set(tab_prefix_map, "C-f", "find_file_other_tab", st)
        call keymap_set(tab_prefix_map, "C-r", "find_file_read_only_other_tab", st)
        ! The repeat maps.
        if (is_keymap_live(tab_bar_switch_repeat_map)) call destroy_keymap(tab_bar_switch_repeat_map, st)
        call make_sparse_keymap(tab_bar_switch_repeat_map)
        call keymap_set(tab_bar_switch_repeat_map, "o", "next_tab", st)
        call keymap_set(tab_bar_switch_repeat_map, "O", "previous_tab", st)
        call set_repeat_map(tab_bar_switch_repeat_map)
        if (is_keymap_live(tab_bar_move_repeat_map)) call destroy_keymap(tab_bar_move_repeat_map, st)
        call make_sparse_keymap(tab_bar_move_repeat_map)
        call keymap_set(tab_bar_move_repeat_map, "m", "move_tab", st)
        call keymap_set(tab_bar_move_repeat_map, "M", "move_tab_bar_tab_backward", st)
        call set_repeat_map(tab_bar_move_repeat_map)
        if (is_keymap_live(tab_bar_history_repeat_map)) call destroy_keymap(tab_bar_history_repeat_map, st)
        call make_sparse_keymap(tab_bar_history_repeat_map)
        call keymap_set(tab_bar_history_repeat_map, "<left>", "go_back_in_tab_bar_history", st)
        call keymap_set(tab_bar_history_repeat_map, "<right>", "go_forward_in_tab_bar_history", st)
        call set_repeat_map(tab_bar_history_repeat_map)
        get_tab_bar_items => get_tab_bar_runs
        close_tab_hook => close_tab_after_last_window
    end subroutine init_tab_bar
end module tab_bar_mod
