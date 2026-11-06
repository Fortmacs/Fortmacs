! window_move_mod.f90 -- directional window-selection routines.
! Ported from GNU Emacs 31.1 lisp/windmove.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> This package defines a set of routines, windmove_{left,up,right,
!> down}, for selection of windows in a frame geometrically.  For
!> example, `select_window_right' selects the window immediately to the right
!> of the currently-selected one.  The command
!> `windmove_default_keybindings' binds them to shift-{left,up,right,down}
!> (in `windmove_mode_map'), and `windmove_delete_default_keybindings'
!> binds the commands that delete the window in a direction.
!>
!> Directions are window_mod's DIRECTION_ABOVE (up), DIRECTION_BELOW
!> (down), DIRECTION_LEFT and DIRECTION_RIGHT.  Deferred: see
!> docs/status/window_move_mod.md.
module window_move_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, USER_ERROR_CODE
    use handle_mod, only: handle_type
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW
    use commands_mod, only: commands
    use hooks_mod, only: hook_type
    use modes_mod, only: minor_modes
    use value_mod, only: value_type, list_elements, make_keymap_reference, symbols, xsymbol, command_type
    use data_mod, only: logical_setting_type
    use keymap_mod, only: keymap_set, define_key, where_is_internal, key_modifiers_type, key_parse, key_binding, lookup_key
    use execute_interactively_mod, only: command_of
    use editing_functions_mod, only: format_arguments_type
    use warnings_mod, only: warn
    use buffer_mod, only: close_buffer
    use minibuffer_reading_mod, only: minibuf_level
    use frame_mod, only: selected_frame
    use window_mod, only: window_in_direction, windows, split_window, select_window, delete_window, is_window_live, &
                          DIRECTION_ABOVE, DIRECTION_BELOW, DIRECTION_LEFT, DIRECTION_RIGHT, MINIBUF_YES, &
                          MINIBUF_NO
    implicit none(type, external)
    private

    public :: windmove_find_other_window, windmove_do_window_select, select_window_left, select_window_up, select_window_right, &
              select_window_down, windmove_mode, windmove_default_keybindings, &
              windmove_delete_in_direction, delete_window_left, delete_window_up, delete_window_right, &
              delete_window_down, windmove_delete_default_keybindings, initialize_windmove
    public :: windmove_create_window_procedure

    abstract interface
        !> A function `windmove_do_window_select' calls to make a window when
        !> there is none in DIRECTION as seen from WINDOW (see
        !> `windmove_create_window%procedure'): ARGUMENT is the numeric prefix
        !> argument (absent for none).  It returns in NEW the window to select, or sets
        !> NO_SELECT to select none.
        subroutine windmove_create_window_procedure(direction, window, new, no_select, status, argument)
            import :: handle_type, int64, status_type
            implicit none(type, external)
            integer, intent(in) :: direction
            type(handle_type), intent(in) :: window
            type(handle_type), intent(out) :: new
            logical, intent(out) :: no_select
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: argument
        end subroutine windmove_create_window_procedure
    end interface

    !> The setting `windmove_create_window': a logical setting whose value
    !> can also be a procedure (R6-37), set with `=>'.  Not a port.
    type, extends(logical_setting_type), public :: windmove_create_window_logical_setting_type
        procedure(windmove_create_window_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => windmove_create_window_clear_kinds
        procedure :: other_value => windmove_create_window_other_value
    end type windmove_create_window_logical_setting_type

    !> Whether movement off the edge of the frame wraps around.
    !> If this variable is set to true, moving left from the leftmost window in
    !> a frame will find the rightmost one, and similarly for the other
    !> directions.  The minibuffer is skipped over in up/down movements if it
    !> is inactive.  Port of windmove.el windmove-wrap-around.
    logical, public :: windmove_wrap_around = .false.

    !> Whether movement off the edge of the frame creates a new window.
    !> If this variable is set to true, moving left from the leftmost window in
    !> a frame will create a new window on the left, and similarly for the other
    !> directions.
    !> This variable may also be a function to be called in this circumstance
    !> by `windmove_do_window_select': point `windmove_create_window%procedure'
    !> at it.  The function should accept then as argument the DIRECTION
    !> targeted, a numeric prefix ARGUMENT and a WINDOW corresponding to the
    !> currently selected window.  It should also return a valid window
    !> that `windmove_do_window_select' will select, or NO_SELECT to
    !> ignore that final selection.
    !> Port of windmove.el windmove-create-window.
    type(windmove_create_window_logical_setting_type), public :: windmove_create_window = &
                                                          windmove_create_window_logical_setting_type(name="windmove_create_window")

    !> Whether the windmove commands are allowed to target all type of windows.
    !> If this variable is set to true, all windmove commands will
    !> ignore the `no_other_window' parameter applied by `display_buffer_rules'
    !> or `set_window_parameter'.  Port of windmove.el
    !> windmove-allow-all-windows (no window has that parameter yet:
    !> window parameters are Deferred).
    logical, public :: windmove_allow_all_windows = .false.

    !> Hook run after entering or leaving `windmove_mode'.  Port of
    !> windmove.el windmove-mode-hook (define-minor-mode's).
    type(hook_type), target, public :: windmove_mode_hook = hook_type(name="windmove_mode_hook")

    !> Map used by `install_windmove_defaults'.  Port of windmove.el
    !> windmove-mode-map.
    type(handle_type), public :: windmove_mode_map

    !> The commands `windmove_default_keybindings' binds, and their keys.
    character(len=*), parameter :: MOVE_COMMANDS(4) = [character(len=24) :: "select_window_left", "select_window_right", &
                                                                                           "select_window_up", "select_window_down"]
    character(len=*), parameter :: DELETE_COMMANDS(4) = [character(len=24) :: "delete_window_left", &
                                                                                        "delete_window_right", "delete_window_up", &
                                                                                                               "delete_window_down"]
    character(len=*), parameter :: ARROW_KEYS(4) = [character(len=8) :: "<left>", "<right>", "<up>", "<down>"]

contains

    !> The name of DIRECTION in messages.  Not a port.
    function direction_name(direction) result(name)
        integer, intent(in) :: direction
        character(len=:), allocatable :: name

        select case (direction)
        case (DIRECTION_ABOVE)
            name = "up"
        case (DIRECTION_BELOW)
            name = "down"
        case (DIRECTION_LEFT)
            name = "left"
        case default
            name = "right"
        end select
    end function direction_name

    !> Whether window H is the minibuffer window and not active.
    !> Not a port.
    logical function is_inactive_minibuffer_window(h)
        type(handle_type), intent(in) :: h

        is_inactive_minibuffer_window = .false.
        if (.not. is_window_live(h)) return
        is_inactive_minibuffer_window = windows(h%index)%mini .and. minibuf_level == 0
    end function is_inactive_minibuffer_window

    !> Return the window object in direction DIRECTION as seen from WINDOW.
    !> WINDOW must be a live window and defaults to the selected one.
    !> Optional ARGUMENT, if negative, means to use the right or bottom edge of
    !> WINDOW as reference position, instead of `window_point'; if positive,
    !> use the left or top edge of WINDOW as reference point.  Port of
    !> windmove.el:windmove-find-other-window.
    function windmove_find_other_window(direction, argument, window) result(other)
        integer, intent(in) :: direction
        integer(int64), intent(in), optional :: argument
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: other

        other = window_in_direction(direction, window, argument, windmove_wrap_around, MINIBUF_YES)
    end function windmove_find_other_window

    !> Move to the window at direction DIRECTION as seen from WINDOW.
    !> DIRECTION, ARGUMENT, and WINDOW are handled as by `windmove_find_other_window'.
    !> If no window is at direction DIRECTION, an error condition occurs.
    !> If `windmove_create_window%procedure' is set, call that function with
    !> DIRECTION, ARGUMENT and WINDOW.  If `windmove_create_window' is true, try
    !> to create a new window in direction DIRECTION instead.  Port of
    !> windmove.el:windmove-do-window-select (repeating a command to
    !> override `no_other_window' is Deferred with window parameters).
    subroutine windmove_do_window_select(direction, status, argument, window)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: other_window, w
        logical :: no_select

        status = status_type()
        no_select = .false.
        other_window = windmove_find_other_window(direction, argument, window)

        ! Create window if needed.
        if ((windmove_create_window%value .or. associated(windmove_create_window%procedure)) .and. &
            (other_window%index == 0 .or. is_inactive_minibuffer_window(other_window))) then
            if (present(window)) then
                w = window
            else
                w = selected_frame%selected_window
            end if

            if (associated(windmove_create_window%procedure)) then
                call windmove_create_window%procedure(direction, w, other_window, no_select, status, argument)
            else
                call split_window(w, 0, .false., direction == DIRECTION_LEFT .or. direction == DIRECTION_RIGHT, &
                                  other_window, status, &
                                  before=direction == DIRECTION_LEFT .or. direction == DIRECTION_ABOVE)
            end if

            if (.not. status%ok()) return
        end if

        if (no_select) return

        if (other_window%index == 0) then
            call set_error(status, USER_ERROR_CODE, "No window "//direction_name(direction)//" from selected window")
        else if (is_inactive_minibuffer_window(other_window)) then
            call set_error(status, USER_ERROR_CODE, "Minibuffer is inactive")
        else
            call select_window(other_window, status)
        end if
    end subroutine windmove_do_window_select

    !> The numeric value of the prefix argument, as ARGUMENT for the windmove
    !> commands: present only if there is one.  Not a port.
    subroutine do_window_select_with_prefix(direction, status)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call windmove_do_window_select(direction, status)
        else
            call windmove_do_window_select(direction, status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine do_window_select_with_prefix

    !> Select the window to the left of the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "left" is relative to the position of point in the window; otherwise
    !> it is relative to the top edge (for positive ARGUMENT) or the bottom edge
    !> (for negative ARGUMENT) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-left.
    recursive subroutine select_window_left(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_LEFT, status)
    end subroutine select_window_left

    !> Select the window above the current one.
    !> With no prefix argument, or with prefix argument equal to zero, "up"
    !> is relative to the position of point in the window; otherwise it is
    !> relative to the left edge (for positive ARGUMENT) or the right edge (for
    !> negative ARGUMENT) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-up.
    recursive subroutine select_window_up(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_ABOVE, status)
    end subroutine select_window_up

    !> Select the window to the right of the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "right" is relative to the position of point in the window;
    !> otherwise it is relative to the top edge (for positive ARGUMENT) or the
    !> bottom edge (for negative ARGUMENT) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-right.
    recursive subroutine select_window_right(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_RIGHT, status)
    end subroutine select_window_right

    !> Select the window below the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "down" is relative to the position of point in the window; otherwise
    !> it is relative to the left edge (for positive ARGUMENT) or the right edge
    !> (for negative ARGUMENT) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-down.
    recursive subroutine select_window_down(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_BELOW, status)
    end subroutine select_window_down

    !> Global minor mode for default windmove commands.  Port of
    !> windmove.el:windmove-mode.
    recursive subroutine windmove_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%windmove_mode%command(status)
    end subroutine windmove_mode

    !> The key description of KEY with MODIFIERS (a `key_modifiers_type'),
    !> as event-convert-list makes the event.  Not a port.
    function modified_key(modifiers, key) result(text)
        type(key_modifiers_type), intent(in) :: modifiers
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: text

        ! In the order of key descriptions.
        text = ""
        if (modifiers%alt) text = text//"A-"
        if (modifiers%control) text = text//"C-"
        if (modifiers%hyper) text = text//"H-"
        if (modifiers%meta) text = text//"M-"
        if (modifiers%shift) text = text//"S-"
        if (modifiers%super) text = text//"s-"
        text = text//key
    end function modified_key

    !> Install keys as specified by COMMANDS and KEYS.
    !> Every command is bound to its key appended to MODIFIERS, adding
    !> PREFIX (a key description, "" for none) to the beginning, before
    !> installing the key.  Previous bindings of each command are unbound.
    !> If UNINSTALL is true, just remove the keys of COMMANDS.  Port of
    !> windmove.el:windmove-install-defaults.
    subroutine install_windmove_defaults(prefix, modifiers, commands_to_bind, keys, status, uninstall)
        character(len=*), intent(in) :: prefix, commands_to_bind(:), keys(:)
        type(key_modifiers_type), intent(in) :: modifiers
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: uninstall
        type(value_type) :: found
        type(value_type), allocatable :: seqs(:), events(:)
        type(command_type), pointer :: binding
        type(status_type) :: st
        character(len=:), allocatable :: key
        logical :: removing
        integer :: i, j

        status = status_type()
        removing = .false.
        if (present(uninstall)) removing = uninstall

        do i = 1, size(commands_to_bind)
            call where_is_internal(value_type(name=trim(commands_to_bind(i))), found, keymap=windmove_mode_map)

            if (allocated(found%a)) then
                call list_elements(found, seqs)

                do j = 1, size(seqs)
                    call list_elements(seqs(j), events)
                    call define_key(windmove_mode_map, events, value_type(), status)
                    if (.not. status%ok()) return
                end do
            end if

            if (.not. removing) then
                key = modified_key(modifiers, trim(keys(i)))
                if (len(prefix) > 0) key = prefix//" "//key
                call key_parse(key, events, status)
                if (.not. status%ok()) return
                call key_binding(events, found, st)
                binding => command_of(found)

                if (associated(binding, commands%self_insert_command)) then
                    block
                        type(format_arguments_type) :: arguments
                        call arguments%append(trim(commands_to_bind(i)))
                        call warn("Command %s is shadowing self-insert-key", arguments, status)
                    end block

                    if (.not. status%ok()) return
                end if

                call lookup_key(make_keymap_reference(windmove_mode_map), events, found, st)
                binding => command_of(found)

                if (associated(binding)) then
                    block
                        type(format_arguments_type) :: arguments
                        call arguments%append(symbols(xsymbol(found))%name)
                        call arguments%append(trim(commands_to_bind(i)))
                        call warn("Overriding %s with %s", arguments, status)
                    end block

                    if (.not. status%ok()) return
                end if

                call keymap_set(windmove_mode_map, key, trim(commands_to_bind(i)), status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine install_windmove_defaults

    !> Set up keybindings for `windmove'.
    !> Keybindings are of the form MODIFIERS-{left,right,up,down},
    !> where MODIFIERS is a `key_modifiers_type' (shift, meta, control,
    !> super, hyper, alt), one or more of them true.
    !> If MODIFIERS is `key_modifiers_type()', the keybindings will be
    !> directly bound to the arrow keys.
    !> Default value of MODIFIERS is shift.  Port of
    !> windmove.el:windmove-default-keybindings.
    subroutine windmove_default_keybindings(status, modifiers)
        type(status_type), intent(out) :: status
        type(key_modifiers_type), intent(in), optional :: modifiers
        type(key_modifiers_type) :: with

        with = key_modifiers_type(shift=.true.)
        if (present(modifiers)) with = modifiers
        call install_windmove_defaults("", with, MOVE_COMMANDS, ARROW_KEYS, status)
    end subroutine windmove_default_keybindings

    !> Delete the window at direction DIRECTION.
    !> If prefix ARGUMENT is C-u, also kill the buffer in that window.
    !> With M-0 prefix, delete the selected window and
    !> select the window at direction DIRECTION.
    !> When `windmove_wrap_around' is true, takes the window
    !> from the opposite side of the frame.  Port of
    !> windmove.el:windmove-delete-in-direction.
    subroutine windmove_delete_in_direction(direction, status)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status
        type(handle_type) :: other_window, selected, buffer

        status = status_type()

        ! ARGUMENT is the raw prefix argument: only a number is a SIGN.
        if (current_prefix_argument%kind == PREFIX_NUMBER) then
            other_window = window_in_direction(direction, sign=current_prefix_argument%value, wrap=windmove_wrap_around, &
                                               minibuf=MINIBUF_NO)
        else
            other_window = window_in_direction(direction, wrap=windmove_wrap_around, minibuf=MINIBUF_NO)
        end if

        if (other_window%index == 0) then
            call set_error(status, USER_ERROR_CODE, "No window "//direction_name(direction)//" from selected window")
            return
        end if

        if (current_prefix_argument%kind == PREFIX_RAW .and. current_prefix_argument%value == 4) then
            ! (A copy: closing the buffer changes the window's.)
            buffer = windows(other_window%index)%buffer
            call close_buffer(buffer, status)
            if (.not. status%ok()) return
        end if

        if (.not. (current_prefix_argument%kind == PREFIX_NUMBER .and. current_prefix_argument%value == 0)) then
            call delete_window(other_window, status)
        else
            selected = selected_frame%selected_window
            call delete_window(selected, status)
            if (.not. status%ok()) return
            call select_window(other_window, status)
        end if
    end subroutine windmove_delete_in_direction

    !> Delete the window to the left of the current one.
    !> If prefix ARGUMENT is C-u, delete the selected window and
    !> select the window that was to the left of the current one.  Port of
    !> windmove.el:windmove-delete-left.
    recursive subroutine delete_window_left(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_LEFT, status)
    end subroutine delete_window_left

    !> Delete the window above the current one.
    !> If prefix ARGUMENT is C-u, delete the selected window and
    !> select the window that was above the current one.  Port of
    !> windmove.el:windmove-delete-up.
    recursive subroutine delete_window_up(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_ABOVE, status)
    end subroutine delete_window_up

    !> Delete the window to the right of the current one.
    !> If prefix ARGUMENT is C-u, delete the selected window and
    !> select the window that was to the right of the current one.  Port of
    !> windmove.el:windmove-delete-right.
    recursive subroutine delete_window_right(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_RIGHT, status)
    end subroutine delete_window_right

    !> Delete the window below the current one.
    !> If prefix ARGUMENT is C-u, delete the selected window and
    !> select the window that was below the current one.  Port of
    !> windmove.el:windmove-delete-down.
    recursive subroutine delete_window_down(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_BELOW, status)
    end subroutine delete_window_down

    !> Set up keybindings for directional window deletion.
    !> Keys are bound to commands that delete windows in the specified
    !> direction.  Keybindings are of the form PREFIX MODIFIERS-{left,right,up,down},
    !> where PREFIX is a prefix key (a key description) and MODIFIERS is
    !> a `key_modifiers_type' (shift, meta, control, super, hyper, alt),
    !> one or more of them true.
    !> If PREFIX is "none", no prefix is used.  If MODIFIERS is
    !> `key_modifiers_type()', the keybindings are directly bound to the
    !> arrow keys.
    !> Default value of PREFIX is C-x and MODIFIERS is shift.  Port of
    !> windmove.el:windmove-delete-default-keybindings.
    subroutine windmove_delete_default_keybindings(status, prefix, modifiers)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prefix
        type(key_modifiers_type), intent(in), optional :: modifiers
        type(key_modifiers_type) :: with
        character(len=:), allocatable :: key_prefix

        key_prefix = "C-x"

        if (present(prefix)) then
            key_prefix = prefix
            if (prefix == "none") key_prefix = ""
        end if

        with = key_modifiers_type(shift=.true.)
        if (present(modifiers)) with = modifiers
        call install_windmove_defaults(key_prefix, with, DELETE_COMMANDS, ARROW_KEYS, status)
    end subroutine windmove_delete_default_keybindings

    recursive subroutine execute_windmove_default_keybindings(status)
        type(status_type), intent(out) :: status

        call windmove_default_keybindings(status)
    end subroutine execute_windmove_default_keybindings

    recursive subroutine execute_windmove_delete_default_keybindings(status)
        type(status_type), intent(out) :: status

        call windmove_delete_default_keybindings(status)
    end subroutine execute_windmove_delete_default_keybindings

    !> Define Windmove mode (on, its :init-value) and the commands.
    !> Not a port.
    subroutine initialize_windmove()
        call minor_modes%windmove_mode%define("windmove_mode", global=.true., hook=windmove_mode_hook, keymap=.true.)
        windmove_mode_map = minor_modes%windmove_mode%map
        call minor_modes%windmove_mode%set_state(.true.)
        commands%windmove_mode%execute => windmove_mode
        commands%select_window_left%execute => select_window_left
        commands%select_window_up%execute => select_window_up
        commands%select_window_right%execute => select_window_right
        commands%select_window_down%execute => select_window_down
        commands%windmove_default_keybindings%execute => execute_windmove_default_keybindings
        commands%delete_window_left%execute => delete_window_left
        commands%delete_window_up%execute => delete_window_up
        commands%delete_window_right%execute => delete_window_right
        commands%delete_window_down%execute => delete_window_down
        commands%windmove_delete_default_keybindings%execute => execute_windmove_delete_default_keybindings
    end subroutine initialize_windmove

    !> Forget the procedure of `windmove_create_window', as any assignment
    !> to it does.  Not a port.
    subroutine windmove_create_window_clear_kinds(self)
        class(windmove_create_window_logical_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine windmove_create_window_clear_kinds

    !> The procedure of `windmove_create_window' as help shows it.
    !> Not a port.
    !> port.
    function windmove_create_window_other_value(self) result(text)
        class(windmove_create_window_logical_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function windmove_create_window_other_value
end module window_move_mod
