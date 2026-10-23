! novice_fm.f90 -- handling of disabled commands ("novice mode").
! Ported from GNU Emacs 31.1 lisp/novice.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Disabled commands: a command whose symbol has a non-nil `disabled`
!> property asks before it runs (`disabled-command-function`), as
!> simple.el:command-execute arranges.  `enable-command` and
!> `disable-command` change the property for the session; to make the
!> choice permanent, call `enable_command` or `disable_command` from the
!> configuration (upstream edits the Lisp init file instead).
module novice_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use lisp_fm, only: value_type, nil, nilp, symbolp, fixnump, xfixnum, t_value
    use lread_fm, only: sym
    use data_fm, only: symbol_name
    use fns_fm, only: get, put
    use keymap_fm, only: key_description
    use keyboard_fm, only: this_command_keys, disabled_command_hook
    use callint_fm, only: defcommand, call_interactively, commandp
    use character_fm, only: CHAR_META
    use help_fm, only: substitute_command_keys, documentation_hook
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use minibuf_fm, only: completion_obarray_table
    use rmc_fm, only: rmc_choice_type, read_multiple_choice
    implicit none(type, external)
    private

    public :: disabled_command_function, enable_command, disable_command, syms_of_novice

    !> Function to call to handle disabled commands.
    !> If nil, the feature is disabled, i.e., all commands work normally.
    !> Port of novice.el disabled-command-function (true is the default
    !> function, false is nil).
    logical, public :: disabled_command_function_p = .true.

    character(len=*), parameter :: LF = achar(10)

contains

    !> The hook keyboard_fm runs before each command (command-execute's
    !> test of the `disabled` property).
    recursive subroutine check_disabled(cmd, handled, status)
        type(value_type), intent(in) :: cmd
        logical, intent(out) :: handled
        type(status_type), intent(out) :: status
        type(value_type) :: disabled
        handled = .false.
        status = status_type()
        if (.not. disabled_command_function_p .or. .not. symbolp(cmd) .or. nilp(cmd)) return
        call get(cmd, sym("disabled"), disabled, status)
        if (.not. status%ok() .or. nilp(disabled)) return
        handled = .true.
        call disabled_command_function(cmd, this_command_keys, status)
    end subroutine check_disabled

    !> Ask whether to run the disabled command CMD, typed as KEYS: n (or
    !> C-g) cancels, y enables it (for the session), SPC runs it this
    !> once, ! enables all disabled commands for the session.  Port of
    !> novice.el:disabled-command-function (no init file to update).
    recursive subroutine disabled_command_function(cmd, keys, status)
        type(value_type), intent(in) :: cmd
        type(value_type), intent(in) :: keys(:)
        type(status_type), intent(out) :: status
        type(rmc_choice_type) :: choices(4)
        type(value_type) :: disabled
        character(len=:), allocatable :: name, help_string, doc
        logical :: by_name, found
        integer :: answer, at
        call symbol_name(cmd, name, status)
        if (.not. status%ok()) return
        by_name = .false.
        if (size(keys) >= 1) then
            if (fixnump(keys(1))) by_name = xfixnum(keys(1)) == ior(CHAR_META, iachar("x"))
        end if
        if (size(keys) >= 2 .and. .not. by_name) then
            if (fixnump(keys(1)) .and. fixnump(keys(2))) then
                by_name = xfixnum(keys(1)) == 27 .and. xfixnum(keys(2)) == iachar("x")
            end if
        end if
        if (by_name .or. size(keys) == 0) then
            help_string = "You have invoked the disabled command "//name//"."//LF
        else
            help_string = "You have typed "//key_description(keys)//", invoking disabled command "//name//"."//LF
        end if
        ! Any special message saying why the command is disabled.
        call get(cmd, sym("disabled"), disabled, status)
        found = .false.
        if (associated(documentation_hook)) call documentation_hook(name, doc, found)
        if (found .and. len(doc) > 0) then
            doc = substitute_command_keys(doc)
        else
            doc = "<< not documented >>"
        end if
        ! Keep only the first paragraph of the documentation.
        at = index(doc, LF//LF)
        if (at > 0) doc = doc(1:at - 1)
        help_string = help_string//"It is disabled because new users often find it confusing."//LF// &
                      "Here's the first part of its description:"//LF//LF//indent(doc, 3)// &
                      LF//LF//"Do you want to use this command anyway?"//LF//LF// &
                      "You can now type:"//LF// &
                      " n    (also C-g) to cancel--don't try the command; it remains disabled."//LF// &
                      " y    to enable the command (no questions if you use it again)."//LF// &
                      " SPC  to try the command just this once, but leave it disabled."//LF// &
                      " !    to enable it and all the disabled commands for this session."
        choices(1)%key = iachar("n")
        choices(1)%name = "no"
        choices(2)%key = iachar("y")
        choices(2)%name = "yes"
        choices(3)%key = iachar(" ")
        choices(3)%name = "(space bar) only once"
        choices(4)%key = iachar("!")
        choices(4)%name = "use and enable all"
        call read_multiple_choice("Use this command?", choices, answer, status, help_string, "*Disabled Command*")
        if (.not. status%ok()) return
        select case (answer)
        case (4)
            disabled_command_function_p = .false.
        case (2)
            call put(cmd, sym("disabled"), nil(), status)
            if (.not. status%ok()) return
        case default
            continue
        end select
        if (answer /= 1) call call_interactively(cmd, status)
    end subroutine disabled_command_function

    !> TEXT with N spaces before each nonempty line (indent-rigidly).
    function indent(text, n) result(indented)
        character(len=*), intent(in) :: text
        integer, intent(in) :: n
        character(len=:), allocatable :: indented
        integer :: i
        logical :: bol
        indented = ""
        bol = .true.
        do i = 1, len(text)
            if (bol .and. text(i:i) /= LF) indented = indented//repeat(" ", n)
            indented = indented//text(i:i)
            bol = text(i:i) == LF
        end do
    end function indent

    !> Allow COMMAND to be executed without special confirmation from now on.
    !> COMMAND must be a symbol.  Port of novice.el:enable-command (for
    !> this session: call it from the configuration to make it last).
    subroutine enable_command(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        call en_disable_command(command, .false., status)
    end subroutine enable_command

    !> Require special confirmation to execute COMMAND from now on.
    !> COMMAND must be a symbol.  Port of novice.el:disable-command (for
    !> this session: call it from the configuration to make it last).
    subroutine disable_command(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        call en_disable_command(command, .true., status)
    end subroutine disable_command

    !> Port of novice.el:en/disable-command (no init file).
    subroutine en_disable_command(command, disable, status)
        character(len=*), intent(in) :: command
        logical, intent(in) :: disable
        type(status_type), intent(out) :: status
        if (.not. commandp(sym(command))) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid command name `"//command//"'")
            return
        end if
        if (disable) then
            call put(sym(command), sym("disabled"), t_value(), status)
        else
            call put(sym(command), sym("disabled"), nil(), status)
        end if
    end subroutine en_disable_command

    ! Read a command name, as interactive "C".
    recursive subroutine read_command(prompt, name, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        call completing_read(prompt, completion_obarray_table(), name, status, require_match=REQUIRE_MATCH_T)
    end subroutine read_command

    recursive subroutine cmd_enable_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call read_command("Enable command: ", name, status)
        if (status%ok()) call enable_command(name, status)
    end subroutine cmd_enable_command

    recursive subroutine cmd_disable_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call read_command("Disable command: ", name, status)
        if (status%ok()) call disable_command(name, status)
    end subroutine cmd_disable_command

    !> Install the check for disabled commands, disable the commands
    !> upstream disables (subr.el `upcase-region` and `downcase-region`;
    !> bindings.el `narrow-to-region`, if defined) and register
    !> `enable-command` and `disable-command`.
    subroutine syms_of_novice()
        type(status_type) :: st
        disabled_command_hook => check_disabled
        call defcommand("enable-command", cmd_enable_command)
        call defcommand("disable-command", cmd_disable_command)
        call put(sym("upcase-region"), sym("disabled"), t_value(), st)
        call put(sym("downcase-region"), sym("disabled"), t_value(), st)
        if (commandp(sym("narrow-to-region"))) call put(sym("narrow-to-region"), sym("disabled"), t_value(), st)
    end subroutine syms_of_novice
end module novice_fm
