! electric_mod.f90 -- electric keys: automatic reindentation.
! Ported from GNU Emacs 31.1 lisp/electric.el.
!
! Copyright (C) 1985-1986, 1995, 2001-2026 Free Software Foundation,
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

!> Electric Indent mode: self-inserting one of `electric_indent_characters'
!> (by default a newline) reindents the line, from
!> `post_self_insert_hook'.  The mode is on by default, as upstream
!> (`:init-value t'): bindings_mod turns it on at startup.  C-j is
!> `electric_newline_and_maybe_indent'.
!>
!> Deferred: `electric_indent_functions' (run-hook-with-args-until-success
!> is not ported), `electric_indent_actions' (advice on `paste' and
!> `before_save_hook'), `electric_layout_mode' and
!> `electric_layout_local_mode' with `electric_layout_rules',
!> `electric_quote_mode' and `electric_quote_local_mode' with their
!> variables, and the electric command loop and pop-up window
!> (`Electric_command_loop', `Electric_pop_up_window').
module electric_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: prefix_argument_type, PREFIX_NONE
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xfixnum, vtag, V_INTEGER
    use data_mod, only: logical_setting_type, default_value, set_default, clear_local_variable, buffer_local_value, &
                        append_locals_clearer
    use commands_mod, only: commands
    use hooks_mod, only: hook_type
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use buffer_mod, only: buffers, current, buffer_list, resolve_buffer, barf_if_buffer_read_only, buffer_serial_live
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use editing_functions_mod, only: point, goto_character, is_beginning_of_line, is_end_of_line, character_before, &
                                     line_beginning_position
    use syntax_mod, only: skip_characters_backward
    use editing_commands_mod, only: post_self_insert_hook
    use keyboard_mod, only: last_command_event
    use indent_mod, only: indent_according_to_mode, indent_line_function, indent_line_function_name
    use simple_mod, only: newline, newline_and_indent, delete_horizontal_space
    use keymap_mod, only: define_key, key_parse
    use modes_mod, only: minor_modes
    implicit none(type, external)
    private

    public :: electric_indent_characters_value, electric_indent_characters_set_local
    public :: initialize_electric, electric_indent_mode, electric_indent_local_mode, electric_after_character_position, &
              is_electric_indent_can_reindent, electric_indent_post_self_insert_function, &
              electric_indent_just_newline, electric_newline_and_maybe_indent

    !> Characters that should cause automatic reindentation.  Port of
    !> electric.el electric-indent-chars (the default value; `initialize_electric'
    !> gives it the newline unless the configuration set it; a buffer's own
    !> value is set with `electric_indent_characters_set_local').
    integer, allocatable, public :: electric_indent_characters(:)

    ! One buffer's own `electric_indent_characters'.
    type :: indent_characters_entry_type
        integer(int64) :: serial = 0
        integer, allocatable :: chars(:)
    end type indent_characters_entry_type

    type(indent_characters_entry_type), allocatable :: local_indent_characters(:)

    !> If true, reindentation is not appropriate for this buffer.
    !> This should be set by major modes such as `python_mode' since
    !> Python does not lend itself to fully automatic indentation.  Port of
    !> electric.el electric-indent-inhibit (buffer-local).
    type(logical_setting_type), public :: electric_indent_inhibit = &
                                          logical_setting_type(name="electric_indent_inhibit", value=.false.)

    !> List of indent functions that can't reindent.
    !> If `indent_line_function' is one of those, then `electric_indent_mode' will
    !> not try to reindent lines.  It is normally better to make the major
    !> mode set `electric_indent_inhibit', but this can be used as a workaround.
    !> Port of electric.el electric-indent-functions-without-reindent (by
    !> name).
    character(len=40), allocatable, public :: electric_indent_functions_without_reindent(:)

    !> Hook run after entering or leaving `electric_indent_mode'.  Port of
    !> the electric-indent-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: electric_indent_mode_hook = hook_type(name="electric_indent_mode_hook")

    !> Hook run after entering or leaving `electric_indent_local_mode'.
    !> Port of the electric-indent-local-mode-hook define-minor-mode
    !> defines.
    type(hook_type), target, public :: electric_indent_local_mode_hook = &
                                       hook_type(name="electric_indent_local_mode_hook")

contains

    !> Define Electric Indent mode and its local mode, and bind C-j in
    !> GLOBAL_MAP (as loading electric.el does).  Not a port.
    subroutine initialize_electric(global_map)
        type(handle_type), intent(in) :: global_map
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call minor_modes%electric_indent_mode%define("electric_indent_mode", global=.true., &
                                                     body=electric_indent_mode_body, hook=electric_indent_mode_hook)
        commands%electric_indent_mode%execute => electric_indent_mode
        call minor_modes%electric_indent_local_mode%define("electric_indent_local_mode", &
                                                           body=electric_indent_local_mode_body, &
                                                           hook=electric_indent_local_mode_hook, &
                                                           getter=is_electric_indent_local_mode)
        commands%electric_indent_local_mode%execute => electric_indent_local_mode
        commands%electric_indent_just_newline%execute => electric_indent_just_newline
        commands%electric_newline_and_maybe_indent%execute => electric_newline_and_maybe_indent
        if (.not. allocated(electric_indent_characters)) electric_indent_characters = [10]
        call append_locals_clearer(clear_electric_locals)

        if (.not. allocated(electric_indent_functions_without_reindent)) then
            electric_indent_functions_without_reindent = [character(len=40) :: "indent_relative", &
                                                          "indent_to_left_margin", "indent_relative_maybe", &
                                                          "indent_relative_first_indent_point", "yaml_indent_line"]
        end if

        call key_parse("C-j", key, st)
        call define_key(global_map, key, value_type(name="electric_newline_and_maybe_indent"), st)
    end subroutine initialize_electric

    ! The index of the current buffer's own `electric_indent_characters', 0 if
    ! none.
    ! Not a port.
    integer function indent_characters_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(local_indent_characters)) return
        k = findloc(local_indent_characters%serial, buffers(current)%serial, dim=1)
    end function indent_characters_index

    ! Find or make the current buffer's own `electric_indent_characters' entry,
    ! reusing the entry of a closed buffer; K is its index (0 without a
    ! current buffer).
    ! Not a port.
    subroutine make_indent_characters_entry(k)
        integer, intent(out) :: k
        type(indent_characters_entry_type) :: entry

        k = indent_characters_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(local_indent_characters)) allocate (local_indent_characters(0))

        do k = 1, size(local_indent_characters)
            if (.not. buffer_serial_live(local_indent_characters(k)%serial)) exit
        end do

        if (k > size(local_indent_characters)) local_indent_characters = [local_indent_characters, entry]
        local_indent_characters(k)%serial = buffers(current)%serial
    end subroutine make_indent_characters_entry

    !> The value of `electric_indent_characters' in the current buffer: its own,
    !> else the default.
    !> Not a port.
    function electric_indent_characters_value() result(chars)
        integer, allocatable :: chars(:)
        integer :: k

        k = indent_characters_index()

        if (k > 0) then
            chars = local_indent_characters(k)%chars
        else if (allocated(electric_indent_characters)) then
            chars = electric_indent_characters
        else
            allocate (chars(0))
        end if
    end function electric_indent_characters_value

    !> Give the current buffer its own value CHARS of `electric_indent_characters'
    !> (set_local).
    !> Not a port.
    subroutine electric_indent_characters_set_local(chars)
        integer, intent(in) :: chars(:)
        integer :: k

        call make_indent_characters_entry(k)
        if (k > 0) local_indent_characters(k)%chars = chars
    end subroutine electric_indent_characters_set_local

    ! kill-all-local-variables: forget the current buffer's own
    ! `electric_indent_characters'.
    ! Not a port.
    subroutine clear_electric_locals()
        integer :: k

        k = indent_characters_index()
        if (k > 0) local_indent_characters(k)%serial = 0
    end subroutine clear_electric_locals

    !> The character `last_command_event' is, or -1.  Not a port.
    integer function event_character()
        event_character = -1
        if (vtag(last_command_event) == V_INTEGER) event_character = int(xfixnum(last_command_event))
    end function event_character

    !> Return the position after the char we just inserted.
    !> Returns 0 when we can't find this char.  Port of
    !> electric.el:electric--after-char-pos.
    integer(int64) function electric_after_character_position() result(position)
        integer(int64) :: original_point, moved
        integer :: c
        logical :: found, ok
        type(status_type) :: st

        original_point = point()
        position = original_point
        call character_before(c, found)
        ok = found .and. c == event_character()

        if (.not. ok) then
            moved = skip_characters_backward(" "//achar(9), st)
            position = point()
            call character_before(c, found)
            ok = found .and. c == event_character()

            if (.not. ok) then
                moved = skip_characters_backward(" "//achar(10)//achar(9), st)
                position = point()
                call character_before(c, found)
                ok = found .and. c == event_character()
            end if

            call goto_character(original_point)
        end if

        if (.not. ok) position = 0
    end function electric_after_character_position

    !> Return true if `electric_indent_mode' can performs reindentation.
    !> Port of electric.el:electric-indent-can-reindent-p.
    logical function is_electric_indent_can_reindent() result(can)
        character(len=:), allocatable :: name
        integer :: k

        can = .not. electric_indent_inhibit%value
        if (.not. can) return
        name = indent_line_function_name(indent_line_function%value)

        do k = 1, size(electric_indent_functions_without_reindent)
            if (name == trim(electric_indent_functions_without_reindent(k))) can = .false.
        end do
    end function is_electric_indent_can_reindent

    !> Function that `electric_indent_mode' adds to `post_self_insert_hook'.
    !> This indents if the hook `electric_indent_functions' returns true,
    !> or if a member of `electric_indent_characters' was typed; but not in a string
    !> or comment.  Port of electric.el:electric-indent-post-self-insert-function
    !> (no `electric_indent_functions').
    subroutine electric_indent_post_self_insert_function(status)
        type(status_type), intent(out) :: status
        integer(int64) :: position, original_point, moved, buffer_position
        integer :: ev
        logical :: go, at_newline
        type(handle_type) :: before, saved
        type(status_type) :: st

        status = status_type()
        if (.not. minor_modes%electric_indent_mode%state%value) return
        ev = event_character()

        ! Don't reindent while inserting spaces at beginning of line.
        if (ev == 32 .or. ev == 9) then
            original_point = point()
            moved = skip_characters_backward(" "//achar(9), st)
            go = .not. is_beginning_of_line()
            call goto_character(original_point)
            if (.not. go) return
        end if

        position = electric_after_character_position()
        if (position == 0) return
        if (.not. any(electric_indent_characters_value() == ev)) return
        ! If we error during indent, silently give up since this is an
        ! automatic action that the user didn't explicitly request.
        ! For newline, we want to reindent both lines and basically behave
        ! like reindent-then-newline-and-indent (whose code we hence
        ! copied).
        at_newline = position <= line_beginning_position()

        if (at_newline) then
            call copy_marker_at(position - 1, before, .true., st)
            call copy_marker_at(point(), saved, .false., st)

            if (is_electric_indent_can_reindent()) then
                ! Don't reindent the previous line if the indentation
                ! function is not a real one.
                call marker_position(before, buffer_position, st)
                call goto_character(buffer_position)
                call indent_according_to_mode(st)

                if (.not. st%ok()) then
                    call restore()
                    return
                end if
            end if

            ! The goal here will be to remove the trailing whitespace after
            ! reindentation of the previous line because that may have
            ! (re)introduced it.
            call marker_position(before, buffer_position, st)
            call goto_character(buffer_position)
            ! We were at END_OF_LINE in marker `before' before the call to
            ! `indent_according_to_mode' but after we may not be
            ! (Bug#15767).
            if (is_end_of_line()) call delete_horizontal_space(.true., status)
            call restore()
            if (.not. status%ok()) return
        end if

        if (electric_indent_inhibit%value .and. .not. at_newline) return
        call indent_according_to_mode(st)

    contains

        !> End the save-excursion and free the markers.
        subroutine restore()
            integer(int64) :: p

            call marker_position(saved, p, st)
            call goto_character(p)
            call destroy_marker(saved, st)
            call destroy_marker(before, st)
        end subroutine restore
    end subroutine electric_indent_post_self_insert_function

    !> Insert just a newline, without any auto-indentation.  Port of
    !> electric.el:electric-indent-just-newline ("*P").
    subroutine electric_indent_just_newline(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        saved = minor_modes%electric_indent_mode%state%value
        minor_modes%electric_indent_mode%state%value = .false.

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call newline(status, interactive=.true.)
        else
            call newline(status, prefix_numeric_value(current_prefix_argument), interactive=.true.)
        end if

        minor_modes%electric_indent_mode%state%value = saved
    end subroutine electric_indent_just_newline

    !> Insert a newline.
    !> If `electric_indent_mode' is enabled, that's that, but if it
    !> is *disabled* then additionally indent according to major mode.
    !> Indentation is done using the value of `indent_line_function'.
    !> In programming language modes, this is the same as TAB.
    !> In some text modes, where TAB inserts a tab, this command indents to the
    !> column specified by the function `current_left_margin'.  Port of
    !> electric.el:electric-newline-and-maybe-indent.
    subroutine electric_newline_and_maybe_indent(status)
        type(status_type), intent(out) :: status
        type(prefix_argument_type) :: saved

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return

        if (minor_modes%electric_indent_mode%state%value) then
            saved = current_prefix_argument
            current_prefix_argument = prefix_argument_type()
            call electric_indent_just_newline(status)
            current_prefix_argument = saved
        else
            call newline_and_indent(status)
        end if
    end subroutine electric_newline_and_maybe_indent

    !> Toggle on-the-fly reindentation of text lines (Electric Indent mode).
    !>
    !> When enabled, this reindents whenever the hook `electric_indent_functions'
    !> returns true, or if you insert one of the "electric characters".
    !> The electric characters normally include the newline, but can
    !> also include other characters as needed by the major mode; see
    !> `electric_indent_characters' for the actual list.
    !>
    !> By "reindent" we mean remove any existing indentation, and then
    !> indent the line according to context and rules of the major mode.
    !>
    !> This is a global minor mode.  To toggle the mode in a single buffer,
    !> use `electric_indent_local_mode'.  Port of
    !> electric.el:electric-indent-mode.
    subroutine electric_indent_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%electric_indent_mode%command(status)
    end subroutine electric_indent_mode

    !> The body of Electric Indent mode.  Port of
    !> electric.el:electric-indent-mode (its body).
    subroutine electric_indent_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: b
        integer :: k
        logical :: found

        status = status_type()
        if (on) continue

        if (.not. minor_modes%electric_indent_mode%state%value) then
            found = .false.
            all = buffer_list()

            do k = 1, size(all)
                call resolve_buffer(all(k), b, st)
                if (.not. st%ok()) cycle
                call buffer_local_value(value_type(name="electric_indent_mode"), b, v, st)

                if (st%ok() .and. allocated(v%a)) then
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) call post_self_insert_hook%remove(electric_indent_post_self_insert_function)
        else
            call post_self_insert_hook%append(electric_indent_post_self_insert_function, depth=60)
        end if
    end subroutine electric_indent_mode_body

    !> Toggle `electric_indent_mode' only in this buffer.  Port of
    !> electric.el:electric-indent-local-mode.
    subroutine electric_indent_local_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%electric_indent_local_mode%command(status)
    end subroutine electric_indent_local_mode

    !> The state of Electric Indent Local mode: `electric_indent_mode' in
    !> the current buffer.  Not a port.
    logical function is_electric_indent_local_mode() result(on)
        on = minor_modes%electric_indent_mode%state%value
    end function is_electric_indent_local_mode

    !> The body of Electric Indent Local mode with its new state VALUE: the
    !> setter (`electric_indent_mode' set locally to VALUE), then the body.  Port of
    !> electric.el:electric-indent-local-mode (its body).
    subroutine electric_indent_local_mode_body(value, status)
        logical, intent(in) :: value
        type(status_type), intent(out) :: status
        type(value_type) :: default
        type(status_type) :: st

        status = status_type()
        call minor_modes%electric_indent_mode%state%set_local(value)
        call default_value(value_type(name="electric_indent_mode"), default, st)

        if (minor_modes%electric_indent_mode%state%value .eqv. allocated(default%a)) then
            call clear_local_variable(value_type(name="electric_indent_mode"), st)
        else if (.not. allocated(default%a)) then
            ! Locally enabled, but globally disabled.
            ! Setup the hooks.
            call minor_modes%electric_indent_mode%enable(status)
            ! But keep it globally disabled.
            call set_default(value_type(name="electric_indent_mode"), value_type(), st)
        end if
    end subroutine electric_indent_local_mode_body
end module electric_mod
