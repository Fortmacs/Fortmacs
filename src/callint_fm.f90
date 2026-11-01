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

!> Running commands.  A command is a `command_type' object (lisp_fm):
!> built-in ones are the components of `commands' (commands_fm), others
!> are defined with `define'.  Keymaps bind command names; upstream a
!> command is a function with an `interactive' spec, stored in the
!> name's function cell, and here a command reads its own arguments
!> (the prefix argument is `current_prefix_arg').
!>
!>     commands%forward_char%execute => cmd_forward_char
module callint_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, &
                        FM_VOID_FUNCTION, FM_WRONG_TYPE_ARGUMENT
    use lisp_fm, only: value_type, V_ARRAY, V_STRING, vtag, xsymbol, symbols, V_NULL, &
                       V_NAME
    use lread_fm, only: intern
    use lisp_fm, only: command_type, command_proc, defined_command, defined_command_name_of, value_type
    use commands_fm, only: builtin_command, BUILTIN_COMMAND_NAMES
    implicit none(type, external)
    private

    public :: command_proc, command_type, find_command, is_command, is_command_defined, &
              set_command_disabled, intern_command_names, command_name_of, command_of
    public :: execute_interactively, find_function, prefix_numeric_value, prefix_arg_repr

    !> The kinds of a prefix argument (`prefix_arg_type%kind'): none, a
    !> number (C-u 5, M-5), one or more C-u's alone (value 4, 16, ...), or
    !> a minus sign alone (C-u -, M--).
    integer, parameter, public :: PREFIX_NONE = 0, PREFIX_NUMBER = 1, PREFIX_RAW = 2, PREFIX_MINUS = 3

    !> A raw prefix argument: its kind (PREFIX_NONE, PREFIX_NUMBER,
    !> PREFIX_RAW or PREFIX_MINUS) and, for PREFIX_NUMBER and PREFIX_RAW,
    !> its number (C-u C-u is PREFIX_RAW with 16).
    type, public :: prefix_arg_type
        integer :: kind = PREFIX_NONE
        integer(int64) :: value = 0
    end type prefix_arg_type

    !> Run command CMD: a command object, or the binding of a key (a name,
    !> or a keyboard macro, which is not run here).
    interface execute_interactively
        module procedure execute_interactively_binding, execute_interactively_command
    end interface execute_interactively

    !> The value of the prefix argument for this editing command.
    !> Its kind is PREFIX_NUMBER for a number, PREFIX_MINUS for just a
    !> minus sign as arg, PREFIX_RAW for just one or more C-u's (value 4,
    !> 16, ...), or PREFIX_NONE if no argument has been specified.  Port of
    !> callint.c current-prefix-arg.
    type(prefix_arg_type), public :: current_prefix_arg

    !> The command now being executed.
    !> The command can set this variable; whatever is put here
    !> will be in `last_command' during the following command.  Port of
    !> keyboard.c this-command (kept here for `execute_interactively').
    type(command_type), pointer, public :: this_command => null()

    !> This is like `this_command', but bound recursively.
    !> Code running from (for instance) a minibuffer hook can check this variable
    !> to see what command invoked the current minibuffer.  Port of
    !> keyboard.c current-minibuffer-command (bound by
    !> callint.c:Fcall_interactively).
    type(command_type), pointer, public :: current_minibuffer_command => null()
    !> keyboard.c KVAR prefix_arg: the raw prefix argument for the next
    !> command (set by universal-argument and friends).
    type(prefix_arg_type), public :: prefix_arg
    !> True means you can use the mark even when inactive.
    !> This option makes a difference in Transient Mark mode.
    !> When the option is true, deactivation of the mark
    !> turns off region highlighting, but commands that use the mark
    !> behave as if the mark were still active.  Port of callint.c
    !> mark-even-if-inactive.
    logical, public :: mark_even_if_inactive = .true.

contains

    !> The function named NAME: one defined with `command_type%define',
    !> else a built-in one (`commands'); null if there is none or it has no
    !> procedure.  Not a port.
    function find_command(name) result(p)
        character(len=*), intent(in) :: name
        type(command_type), pointer :: p

        p => defined_command(name)
        if (.not. associated(p)) p => builtin_command(name)

        if (associated(p)) then
            if (.not. associated(p%execute)) p => null()
        end if
    end function find_command

    !> The function that the name V names (a key's binding), or null.
    !> Not a port.
    function command_of(v) result(p)
        type(value_type), intent(in) :: v
        type(command_type), pointer :: p

        p => null()
        if ((vtag(v) /= V_NAME .and. vtag(v) /= V_NULL) .or. .not. allocated(v%a)) return
        p => find_command(symbols(xsymbol(v))%name)
    end function command_of

    !> The name of the function whose procedure is PROC ("" if none).
    !> Not a port.
    function command_name_of(proc) result(name)
        procedure(command_proc) :: proc
        character(len=:), allocatable :: name
        type(command_type), pointer :: p
        integer :: i

        name = defined_command_name_of(proc)
        if (len(name) > 0) return

        do i = 1, size(BUILTIN_COMMAND_NAMES)
            p => builtin_command(trim(BUILTIN_COMMAND_NAMES(i)))
            if (.not. associated(p)) cycle
            if (.not. associated(p%execute)) cycle

            if (associated(p%execute, proc)) then
                name = trim(BUILTIN_COMMAND_NAMES(i))
                return
            end if
        end do
    end function command_name_of

    !> Intern the names of the built-in commands, so that completion over
    !> the name table (M-x) sees them, and give each command object its
    !> name (`command_type%name', which help shows for `this_command').
    !> Not a port.
    subroutine intern_command_names()
        type(command_type), pointer :: p
        integer :: i
        integer(int64) :: id

        do i = 1, size(BUILTIN_COMMAND_NAMES)
            id = intern(trim(BUILTIN_COMMAND_NAMES(i)))
            p => builtin_command(trim(BUILTIN_COMMAND_NAMES(i)))
        end do
    end subroutine intern_command_names

    !> Whether name `v' names a defined command (a function that is a
    !> command).  Not a port.
    logical function is_command_defined(v)
        type(value_type), intent(in) :: v
        type(command_type), pointer :: p

        p => command_of(v)
        is_command_defined = associated(p)
        if (is_command_defined) is_command_defined = p%is_command
    end function is_command_defined

    !> Whether `v' can be run as a command: a defined command, or a
    !> keyboard macro (a string or a list of events).  Port of
    !> eval.c:Fcommandp (for the kinds of commands that exist here).
    logical function is_command(v)
        type(value_type), intent(in) :: v

        select case (vtag(v))
        case (V_STRING)
            is_command = .true.
        case (V_ARRAY)
            is_command = .true.
        case default
            is_command = is_command_defined(v)
        end select
    end function is_command

    !> Make the command named NAME disabled or not (its `disabled'
    !> property), for a name only known at run time; built-in commands
    !> are `commands%NAME%disabled'.  Not a port.
    subroutine set_command_disabled(name, disabled, status)
        character(len=*), intent(in) :: name
        logical, intent(in) :: disabled
        type(status_type), intent(out) :: status
        type(command_type), pointer :: p

        status = status_type()
        p => defined_command(name)
        if (.not. associated(p)) p => builtin_command(name)

        if (.not. associated(p)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Invalid command name `")//name//format_message("'"))
            return
        end if

        p%disabled = disabled
        if (allocated(p%disabled_message)) deallocate (p%disabled_message)
    end subroutine set_command_disabled

    !> Run the command that the binding CMD names.  Port of
    !> callint.c:Fcall_interactively (how the command reads its arguments
    !> is the command's own business here).  Recursive: a command may read
    !> the minibuffer, running commands in turn.
    recursive subroutine execute_interactively_binding(cmd, status)
        type(value_type), intent(in) :: cmd
        type(status_type), intent(out) :: status
        type(command_type), pointer :: p

        p => command_of(cmd)

        if (associated(p)) then
            if (.not. p%is_command) p => null()
        end if

        if (.not. associated(p)) then
            if ((vtag(cmd) == V_NAME .or. vtag(cmd) == V_NULL)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(command_type), got "//symbols(xsymbol(cmd))%name)
            else
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(command_type)")
            end if

            return
        end if

        call execute_interactively_command(p, status)
    end subroutine execute_interactively_binding

    !> Run command CMD.  Port of callint.c:Fcall_interactively (how the
    !> command reads its arguments is the command's own business here).
    !> Recursive: a command may read the minibuffer, running commands in
    !> turn.
    recursive subroutine execute_interactively_command(cmd, status)
        type(command_type), intent(in), target :: cmd
        type(status_type), intent(out) :: status
        type(command_type), pointer :: saved

        status = status_type()

        if (.not. cmd%is_command .or. .not. associated(cmd%execute)) then
            if (allocated(cmd%name)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(command_type), got "//cmd%name)
            else
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(command_type)")
            end if

            return
        end if

        saved => current_minibuffer_command
        current_minibuffer_command => this_command
        call cmd%execute(status)
        current_minibuffer_command => saved
    end subroutine execute_interactively_command

    !> The function named FN, as a pointer P to call directly; a
    !> `void-function' (or `invalid-function') error condition occurs if
    !> there is none.  Port of eval.c:Ffuncall (resolving the function;
    !> the caller calls it).
    subroutine find_function(fn, p, status)
        type(value_type), intent(in) :: fn
        type(command_type), pointer, intent(out) :: p
        type(status_type), intent(out) :: status

        status = status_type()
        p => command_of(fn)

        if (.not. associated(p)) then
            if ((vtag(fn) == V_NAME .or. vtag(fn) == V_NULL)) then
                call set_error(status, FM_VOID_FUNCTION, symbols(xsymbol(fn))%name)
            else
                call set_error(status, FM_GENERIC_ERROR, "Invalid function")
            end if

            return
        end if
    end subroutine find_function

    !> The numeric value of raw prefix argument RAW: no prefix gives 1, a
    !> minus sign alone gives -1, C-u's and a number N give N.  Port of
    !> callint.c:Fprefix_numeric_value.
    elemental integer(int64) function prefix_numeric_value(raw) result(n)
        type(prefix_arg_type), intent(in) :: raw

        select case (raw%kind)
        case (PREFIX_NUMBER, PREFIX_RAW)
            n = raw%value
        case (PREFIX_MINUS)
            n = -1
        case default
            n = 1
        end select
    end function prefix_numeric_value

    !> Raw prefix argument ARG as help shows it, a Fortran structure
    !> constructor: "prefix_arg_type(kind=PREFIX_RAW, value=4)".
    !> Not a port.
    function prefix_arg_repr(arg) result(text)
        type(prefix_arg_type), intent(in) :: arg
        character(len=:), allocatable :: text
        character(len=24) :: buf

        write (buf, "(i0)") arg%value

        select case (arg%kind)
        case (PREFIX_NUMBER)
            text = "prefix_arg_type(kind=PREFIX_NUMBER, value="//trim(buf)//")"
        case (PREFIX_RAW)
            text = "prefix_arg_type(kind=PREFIX_RAW, value="//trim(buf)//")"
        case (PREFIX_MINUS)
            text = "prefix_arg_type(kind=PREFIX_MINUS)"
        case default
            text = "prefix_arg_type(kind=PREFIX_NONE)"
        end select
    end function prefix_arg_repr
end module callint_fm
