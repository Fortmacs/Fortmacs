! callint_fm.f90 -- commands: the registry and calling them.
! Ported from GNU Emacs 31.1 src/callint.c.
!
! Copyright (C) 1985-1986, 1993-1995, 1997, 2000-2026 Free Software
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

!> Commands are Fortran subroutines registered under a symbol, which is
!> what keymaps bind.  Upstream a command is a function with an
!> `interactive` spec, stored in the symbol's function cell; here the
!> registry plays the function cell and a command reads its own
!> arguments (the prefix argument is `current_prefix_arg`).
!>
!>     call defcommand("forward-char", cmd_forward_char)
!>     call call_interactively(sym("forward-char"), status)
module callint_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, &
                        FM_VOID_FUNCTION
    use lisp_fm, only: value_type, V_LIST, V_STRING, nsymbols, vtag, nilp, &
                       fixnump, symbolp, consp, xfixnum, xsymbol, symbols, &
                       list_elt
    use lread_fm, only: intern, sym
    use data_fm, only: value_eq
    implicit none(type, external)
    private

    public :: command_proc, defcommand, commandp, command_defined_p
    public :: call_interactively, prefix_numeric_value

    abstract interface
        !> A command.  It reads the prefix argument from
        !> `current_prefix_arg` and reports errors through `status`.
        subroutine command_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine command_proc
    end interface

    type :: command_type
        procedure(command_proc), pointer, nopass :: proc => null()
    end type command_type

    !> The registry, indexed by symbol id.
    type(command_type), allocatable :: commands(:)

    !> callint.c:Vcurrent_prefix_arg: the raw prefix argument of the
    !> command being run.
    type(value_type), public :: current_prefix_arg
    !> keyboard.c KVAR prefix_arg: the raw prefix argument for the next
    !> command (set by universal-argument and friends).
    type(value_type), public :: prefix_arg

contains

    !> Register `proc` as the command named `name` (replacing any earlier
    !> one).  The counterpart of defining an interactive function.
    subroutine defcommand(name, proc)
        character(len=*), intent(in) :: name
        procedure(command_proc) :: proc
        type(command_type), allocatable :: grown(:)
        integer(int64) :: id

        id = intern(name)
        if (.not. allocated(commands)) allocate (commands(max(256_int64, 2*nsymbols)))

        if (id > size(commands, kind=int64)) then
            allocate (grown(max(id, 2*size(commands, kind=int64))))
            grown(1:size(commands)) = commands
            call move_alloc(grown, commands)
        end if

        commands(id)%proc => proc
    end subroutine defcommand

    !> Whether symbol `v` names a registered command.
    logical function command_defined_p(v)
        type(value_type), intent(in) :: v
        integer(int64) :: id

        command_defined_p = .false.
        if (.not. symbolp(v) .or. .not. allocated(commands)) return
        id = xsymbol(v)
        if (id > size(commands, kind=int64)) return
        command_defined_p = associated(commands(id)%proc)
    end function command_defined_p

    !> Whether `v` can be run as a command: a registered command, or a
    !> keyboard macro (a string or a list of events).  Port of
    !> eval.c:Fcommandp (for the kinds of commands that exist here).
    logical function commandp(v)
        type(value_type), intent(in) :: v

        select case (vtag(v))
        case (V_STRING)
            commandp = .true.
        case (V_LIST)
            commandp = .true.
        case default
            commandp = command_defined_p(v)
        end select
    end function commandp

    !> Run command `cmd`.  Port of callint.c:Fcall_interactively (the
    !> interactive spec is the command's own business here).  Recursive:
    !> a command may read the minibuffer, running commands in turn.
    recursive subroutine call_interactively(cmd, status)
        type(value_type), intent(in) :: cmd
        type(status_type), intent(out) :: status

        if (.not. command_defined_p(cmd)) then
            if (symbolp(cmd)) then
                call set_error(status, FM_VOID_FUNCTION, symbols(xsymbol(cmd))%name)
            else
                call set_error(status, FM_GENERIC_ERROR, "Not a command")
            end if

            return
        end if

        call commands(xsymbol(cmd))%proc(status)
    end subroutine call_interactively

    !> The numeric value of raw prefix argument `raw`: nil gives 1, `-`
    !> gives -1, (N) and N give N.  Port of callint.c:Fprefix_numeric_value.
    integer(int64) function prefix_numeric_value(raw) result(n)
        type(value_type), intent(in) :: raw
        type(value_type) :: head

        n = 1
        if (nilp(raw)) return

        if (value_eq(raw, sym("-"))) then
            n = -1
        else if (consp(raw)) then
            head = list_elt(raw, 1_int64)
            if (fixnump(head)) n = xfixnum(head)
        else if (fixnump(raw)) then
            n = xfixnum(raw)
        end if
    end function prefix_numeric_value
end module callint_fm
