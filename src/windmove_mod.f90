! windmove_mod.f90 -- directional window-selection routines.
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
!> example, `windmove_right' selects the window immediately to the right
!> of the currently-selected one.  The command
!> `windmove_default_keybindings' binds them to shift-{left,up,right,down}
!> (in `windmove_mode_map'), and `windmove_delete_default_keybindings'
!> binds the commands that delete the window in a direction.
!>
!> Directions are window_mod's DIRECTION_ABOVE (up), DIRECTION_BELOW
!> (down), DIRECTION_LEFT and DIRECTION_RIGHT.  Deferred: see the
!> PORTING.md row.
module windmove_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, FM_USER_ERROR
    use handle_mod, only: handle_type
    use callint_mod, only: current_prefix_arg, prefix_numeric_value, PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW
    use commands_mod, only: commands
    use hooks_mod, only: hook_type
    use modes_mod, only: minor_modes
    use value_mod, only: value_type, list_elements
    use keymap_mod, only: keymap_set, define_key, where_is_internal
    use buffer_mod, only: close_buffer
    use minibuf_mod, only: minibuf_level
    use frame_mod, only: selected_frame
    use window_mod, only: window_in_direction, windows, split_window, select_window, delete_window, is_window_live, &
                          DIRECTION_ABOVE, DIRECTION_BELOW, DIRECTION_LEFT, DIRECTION_RIGHT, MINIBUF_YES, &
                          MINIBUF_NO
    implicit none(type, external)
    private

    public :: windmove_find_other_window, windmove_do_window_select, windmove_left, windmove_up, windmove_right, &
              windmove_down, windmove_mode, windmove_default_keybindings, &
              windmove_delete_in_direction, windmove_delete_left, windmove_delete_up, windmove_delete_right, &
              windmove_delete_down, windmove_delete_default_keybindings, init_windmove
    public :: windmove_create_window_procedure

    abstract interface
        !> A function `windmove_do_window_select' calls to make a window when
        !> there is none in DIRECTION as seen from WINDOW (see
        !> `windmove_create_window_function'): ARG is the numeric prefix
        !> argument (absent for none).  It returns in NEW the window to select, or sets
        !> NO_SELECT to select none.
        subroutine windmove_create_window_procedure(direction, window, new, no_select, status, arg)
            import :: handle_type, int64, status_type
            implicit none(type, external)
            integer, intent(in) :: direction
            type(handle_type), intent(in) :: window
            type(handle_type), intent(out) :: new
            logical, intent(out) :: no_select
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
        end subroutine windmove_create_window_procedure
    end interface

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
    !> by `windmove_do_window_select' (`windmove_create_window_function').
    !> Port of windmove.el windmove-create-window (t).
    logical, public :: windmove_create_window = .false.

    !> The function `windmove_do_window_select' calls in the circumstance
    !> of `windmove_create_window', which see (null for none).  The function
    !> should accept then as argument the DIRECTION targeted, a numeric prefix
    !> ARG and a WINDOW corresponding to the currently selected window.  It
    !> should also return a valid window that `windmove_do_window_select'
    !> will select, or NO_SELECT to ignore that final selection.  Port of
    !> windmove.el windmove-create-window (a function).
    procedure(windmove_create_window_procedure), pointer, public :: windmove_create_window_function => null()

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

    !> Map used by `windmove_install_defaults'.  Port of windmove.el
    !> windmove-mode-map.
    type(handle_type), public :: windmove_mode_map

    !> The commands `windmove_default_keybindings' binds, and their keys.
    character(len=*), parameter :: MOVE_COMMANDS(4) = [character(len=24) :: "windmove_left", "windmove_right", &
                                                                                                     "windmove_up", "windmove_down"]
    character(len=*), parameter :: DELETE_COMMANDS(4) = [character(len=24) :: "windmove_delete_left", &
                                                                                    "windmove_delete_right", "windmove_delete_up", &
                                                                                                             "windmove_delete_down"]
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
    !> Optional ARG, if negative, means to use the right or bottom edge of
    !> WINDOW as reference position, instead of `window_point'; if positive,
    !> use the left or top edge of WINDOW as reference point.  Port of
    !> windmove.el:windmove-find-other-window.
    function windmove_find_other_window(direction, arg, window) result(other)
        integer, intent(in) :: direction
        integer(int64), intent(in), optional :: arg
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: other

        other = window_in_direction(direction, window, arg, windmove_wrap_around, MINIBUF_YES)
    end function windmove_find_other_window

    !> Move to the window at direction DIRECTION as seen from WINDOW.
    !> DIRECTION, ARG, and WINDOW are handled as by `windmove_find_other_window'.
    !> If no window is at direction DIRECTION, an error condition occurs.
    !> If `windmove_create_window_function' is set, call that function with
    !> DIRECTION, ARG and WINDOW.  If `windmove_create_window' is true, try
    !> to create a new window in direction DIRECTION instead.  Port of
    !> windmove.el:windmove-do-window-select (repeating a command to
    !> override `no_other_window' is Deferred with window parameters).
    subroutine windmove_do_window_select(direction, status, arg, window)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: other_window, w
        logical :: no_select

        status = status_type()
        no_select = .false.
        other_window = windmove_find_other_window(direction, arg, window)

        ! Create window if needed.
        if ((windmove_create_window .or. associated(windmove_create_window_function)) .and. &
            (other_window%index == 0 .or. is_inactive_minibuffer_window(other_window))) then
            if (present(window)) then
                w = window
            else
                w = selected_frame%selected_window
            end if

            if (associated(windmove_create_window_function)) then
                call windmove_create_window_function(direction, w, other_window, no_select, status, arg)
            else
                call split_window(w, 0, .false., direction == DIRECTION_LEFT .or. direction == DIRECTION_RIGHT, &
                                  other_window, status, &
                                  before=direction == DIRECTION_LEFT .or. direction == DIRECTION_ABOVE)
            end if

            if (.not. status%ok()) return
        end if

        if (no_select) return

        if (other_window%index == 0) then
            call set_error(status, FM_USER_ERROR, "No window "//direction_name(direction)//" from selected window")
        else if (is_inactive_minibuffer_window(other_window)) then
            call set_error(status, FM_USER_ERROR, "Minibuffer is inactive")
        else
            call select_window(other_window, status)
        end if
    end subroutine windmove_do_window_select

    !> The numeric value of the prefix argument, as ARG for the windmove
    !> commands: present only if there is one.  Not a port.
    subroutine do_window_select_with_prefix(direction, status)
        integer, intent(in) :: direction
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call windmove_do_window_select(direction, status)
        else
            call windmove_do_window_select(direction, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine do_window_select_with_prefix

    !> Select the window to the left of the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "left" is relative to the position of point in the window; otherwise
    !> it is relative to the top edge (for positive ARG) or the bottom edge
    !> (for negative ARG) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-left.
    recursive subroutine windmove_left(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_LEFT, status)
    end subroutine windmove_left

    !> Select the window above the current one.
    !> With no prefix argument, or with prefix argument equal to zero, "up"
    !> is relative to the position of point in the window; otherwise it is
    !> relative to the left edge (for positive ARG) or the right edge (for
    !> negative ARG) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-up.
    recursive subroutine windmove_up(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_ABOVE, status)
    end subroutine windmove_up

    !> Select the window to the right of the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "right" is relative to the position of point in the window;
    !> otherwise it is relative to the top edge (for positive ARG) or the
    !> bottom edge (for negative ARG) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-right.
    recursive subroutine windmove_right(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_RIGHT, status)
    end subroutine windmove_right

    !> Select the window below the current one.
    !> With no prefix argument, or with prefix argument equal to zero,
    !> "down" is relative to the position of point in the window; otherwise
    !> it is relative to the left edge (for positive ARG) or the right edge
    !> (for negative ARG) of the current window.
    !> If no window is at the desired location, an error condition occurs
    !> unless `windmove_create_window' is true and a new window is created.
    !> Port of windmove.el:windmove-down.
    recursive subroutine windmove_down(status)
        type(status_type), intent(out) :: status

        call do_window_select_with_prefix(DIRECTION_BELOW, status)
    end subroutine windmove_down

    !> Global minor mode for default windmove commands.  Port of
    !> windmove.el:windmove-mode.
    recursive subroutine windmove_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%windmove_mode%command(status)
    end subroutine windmove_mode

    !> The key description of KEY with the modifiers named in MODIFIERS
    !> ("shift", "meta", "control", "super", "hyper", "alt"; others are
    !> ignored), as event-convert-list makes the event.  Not a port.
    function modified_key(modifiers, key) result(text)
        character(len=*), intent(in) :: modifiers(:), key
        character(len=:), allocatable :: text
        character(len=*), parameter :: NAMES(6) = [character(len=8) :: "alt", "control", "hyper", "meta", "shift", &
                                                                                                                  "super"]
        character(len=*), parameter :: PREFIXES(6) = [character(len=2) :: "A-", "C-", "H-", "M-", "S-", "s-"]
        integer :: k

        text = ""

        ! In the order of key descriptions.
        do k = 1, size(NAMES)
            if (any(modifiers == NAMES(k))) text = text//PREFIXES(k)
        end do

        text = text//key
    end function modified_key

    !> Install keys as specified by COMMANDS and KEYS.
    !> Every command is bound to its key appended to MODIFIERS, adding
    !> PREFIX (a key description, "" for none) to the beginning, before
    !> installing the key.  Previous bindings of each command are unbound.
    !> If UNINSTALL is true, just remove the keys of COMMANDS.  Port of
    !> windmove.el:windmove-install-defaults (the warnings about shadowing
    !> `self_insert_command' or overriding a binding are Deferred:
    !> warnings.el is not ported).
    subroutine windmove_install_defaults(prefix, modifiers, commands_to_bind, keys, status, uninstall)
        character(len=*), intent(in) :: prefix, modifiers(:), commands_to_bind(:), keys(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: uninstall
        type(value_type) :: found
        type(value_type), allocatable :: seqs(:), events(:)
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
                call keymap_set(windmove_mode_map, key, trim(commands_to_bind(i)), status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine windmove_install_defaults

    !> The modifiers of a default-keybindings procedure: the ones given, or
    !> DEFAULT (a list) when none is; "none" first means no modifiers.
    !> Not a port.
    subroutine modifiers_of(default, modifiers, modifier, modifier_2, modifier_3, modifier_4, modifier_5, modifier_6)
        character(len=*), intent(in) :: default(:)
        character(len=8), allocatable, intent(out) :: modifiers(:)
        character(len=*), intent(in), optional :: modifier, modifier_2, modifier_3, modifier_4, modifier_5, modifier_6

        if (.not. present(modifier)) then
            modifiers = [character(len=8) :: default]
            return
        end if

        allocate (modifiers(0))
        if (modifier == "none") return
        modifiers = [character(len=8) :: modifier]
        if (present(modifier_2)) modifiers = [character(len=8) :: modifiers, modifier_2]
        if (present(modifier_3)) modifiers = [character(len=8) :: modifiers, modifier_3]
        if (present(modifier_4)) modifiers = [character(len=8) :: modifiers, modifier_4]
        if (present(modifier_5)) modifiers = [character(len=8) :: modifiers, modifier_5]
        if (present(modifier_6)) modifiers = [character(len=8) :: modifiers, modifier_6]
    end subroutine modifiers_of

    !> Set up keybindings for `windmove'.
    !> Keybindings are of the form MODIFIERS-{left,right,up,down},
    !> where MODIFIERS are the modifiers given ("shift", "meta", "control",
    !> "super", "hyper" or "alt"), one or more.
    !> If MODIFIER is "none", the keybindings will be directly bound to
    !> the arrow keys.
    !> Default value of MODIFIERS is "shift".  Port of
    !> windmove.el:windmove-default-keybindings.
    subroutine windmove_default_keybindings(status, modifier, modifier_2, modifier_3, modifier_4, modifier_5, modifier_6)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: modifier, modifier_2, modifier_3, modifier_4, modifier_5, modifier_6
        character(len=8), allocatable :: modifiers(:)

        call modifiers_of([character(len=8) :: "shift"], modifiers, modifier, modifier_2, modifier_3, modifier_4, &
                          modifier_5, modifier_6)
        call windmove_install_defaults("", modifiers, MOVE_COMMANDS, ARROW_KEYS, status)
    end subroutine windmove_default_keybindings

    !> Delete the window at direction DIRECTION.
    !> If prefix ARG is C-u, also kill the buffer in that window.
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

        ! ARG is the raw prefix argument: only a number is a SIGN.
        if (current_prefix_arg%kind == PREFIX_NUMBER) then
            other_window = window_in_direction(direction, sign=current_prefix_arg%value, wrap=windmove_wrap_around, &
                                               minibuf=MINIBUF_NO)
        else
            other_window = window_in_direction(direction, wrap=windmove_wrap_around, minibuf=MINIBUF_NO)
        end if

        if (other_window%index == 0) then
            call set_error(status, FM_USER_ERROR, "No window "//direction_name(direction)//" from selected window")
            return
        end if

        if (current_prefix_arg%kind == PREFIX_RAW .and. current_prefix_arg%value == 4) then
            ! (A copy: closing the buffer changes the window's.)
            buffer = windows(other_window%index)%buffer
            call close_buffer(buffer, status)
            if (.not. status%ok()) return
        end if

        if (.not. (current_prefix_arg%kind == PREFIX_NUMBER .and. current_prefix_arg%value == 0)) then
            call delete_window(other_window, status)
        else
            selected = selected_frame%selected_window
            call delete_window(selected, status)
            if (.not. status%ok()) return
            call select_window(other_window, status)
        end if
    end subroutine windmove_delete_in_direction

    !> Delete the window to the left of the current one.
    !> If prefix ARG is C-u, delete the selected window and
    !> select the window that was to the left of the current one.  Port of
    !> windmove.el:windmove-delete-left.
    recursive subroutine windmove_delete_left(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_LEFT, status)
    end subroutine windmove_delete_left

    !> Delete the window above the current one.
    !> If prefix ARG is C-u, delete the selected window and
    !> select the window that was above the current one.  Port of
    !> windmove.el:windmove-delete-up.
    recursive subroutine windmove_delete_up(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_ABOVE, status)
    end subroutine windmove_delete_up

    !> Delete the window to the right of the current one.
    !> If prefix ARG is C-u, delete the selected window and
    !> select the window that was to the right of the current one.  Port of
    !> windmove.el:windmove-delete-right.
    recursive subroutine windmove_delete_right(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_RIGHT, status)
    end subroutine windmove_delete_right

    !> Delete the window below the current one.
    !> If prefix ARG is C-u, delete the selected window and
    !> select the window that was below the current one.  Port of
    !> windmove.el:windmove-delete-down.
    recursive subroutine windmove_delete_down(status)
        type(status_type), intent(out) :: status

        call windmove_delete_in_direction(DIRECTION_BELOW, status)
    end subroutine windmove_delete_down

    !> Set up keybindings for directional window deletion.
    !> Keys are bound to commands that delete windows in the specified
    !> direction.  Keybindings are of the form PREFIX MODIFIERS-{left,right,up,down},
    !> where PREFIX is a prefix key (a key description) and MODIFIERS are
    !> the modifiers given ("shift", "meta", "control", "super", "hyper" or
    !> "alt"), one or more.
    !> If PREFIX is "none", no prefix is used.  If MODIFIER is "none",
    !> the keybindings are directly bound to the arrow keys.
    !> Default value of PREFIX is C-x and MODIFIERS is "shift".  Port of
    !> windmove.el:windmove-delete-default-keybindings.
    subroutine windmove_delete_default_keybindings(status, prefix, modifier, modifier_2, modifier_3, modifier_4, &
                                                   modifier_5, modifier_6)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prefix, modifier, modifier_2, modifier_3, modifier_4, modifier_5, &
                                                  modifier_6
        character(len=8), allocatable :: modifiers(:)
        character(len=:), allocatable :: key_prefix

        key_prefix = "C-x"

        if (present(prefix)) then
            key_prefix = prefix
            if (prefix == "none") key_prefix = ""
        end if

        call modifiers_of([character(len=8) :: "shift"], modifiers, modifier, modifier_2, modifier_3, modifier_4, &
                          modifier_5, modifier_6)
        call windmove_install_defaults(key_prefix, modifiers, DELETE_COMMANDS, ARROW_KEYS, status)
    end subroutine windmove_delete_default_keybindings

    recursive subroutine cmd_windmove_default_keybindings(status)
        type(status_type), intent(out) :: status

        call windmove_default_keybindings(status)
    end subroutine cmd_windmove_default_keybindings

    recursive subroutine cmd_windmove_delete_default_keybindings(status)
        type(status_type), intent(out) :: status

        call windmove_delete_default_keybindings(status)
    end subroutine cmd_windmove_delete_default_keybindings

    !> Define Windmove mode (on, its :init-value) and the commands.
    !> Not a port.
    subroutine init_windmove()
        call minor_modes%windmove_mode%define("windmove_mode", global=.true., hook=windmove_mode_hook, keymap=.true.)
        windmove_mode_map = minor_modes%windmove_mode%map
        call minor_modes%windmove_mode%set_state(.true.)
        commands%windmove_mode%execute => windmove_mode
        commands%windmove_left%execute => windmove_left
        commands%windmove_up%execute => windmove_up
        commands%windmove_right%execute => windmove_right
        commands%windmove_down%execute => windmove_down
        commands%windmove_default_keybindings%execute => cmd_windmove_default_keybindings
        commands%windmove_delete_left%execute => windmove_delete_left
        commands%windmove_delete_up%execute => windmove_delete_up
        commands%windmove_delete_right%execute => windmove_delete_right
        commands%windmove_delete_down%execute => windmove_delete_down
        commands%windmove_delete_default_keybindings%execute => cmd_windmove_delete_default_keybindings
    end subroutine init_windmove
end module windmove_mod
