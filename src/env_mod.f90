! env_mod.f90 -- functions to manipulate environment variables.
! Ported from GNU Emacs 31.1 lisp/env.el.
!
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> Reading and setting environment variables, and substituting them into
!> strings.  `getenv' and `setenv' work on callproc_mod's
!> `process_environment', the environment subprocesses inherit:
!>
!>     call setenv("TERM", "dumb")
!>     call getenv("TERM", value, found)
!>
!> Until the editor's own reads of its environment all go through
!> `getenv' (upstream's C code reads `process-environment' with
!> egetenv; some ported code still calls get_environment_variable),
!> `setenv' also sets or removes the variable in the editor's process,
!> so those reads see the change too.
!>
!> Deferred: the commands M-x getenv and M-x setenv (reading the name
!> with completion over `process_environment', `setenv''s history and
!> prefix argument) and `setenv''s SUBSTITUTE-ENV-VARS, which only they
!> use; a frame's environment (FRAME); encoding with
!> `locale-coding-system' (variables and values are UTF-8 as given);
!> `set-time-zone-rule' when TZ is set (TZ is only changed in the
!> environment); `with-environment-variables' (save, `setenv', run,
!> restore).
module env_mod
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR
    use callproc_mod, only: getenv_internal, setenv_internal, GETENV_SET
    use sysdep_mod, only: sys_setenv, sys_unsetenv
    implicit none(type, external)
    private

    public :: getenv, setenv, substitute_env_vars

contains

    !> Get the value of environment variable VARIABLE.
    !> `found' is false if VARIABLE is undefined in the environment (and
    !> `value' is then "").  Otherwise, `value' is a string.
    !>
    !> This function searches `process_environment' for
    !> VARIABLE and returns its value if found.  If not found, and VARIABLE
    !> is "DISPLAY", the function searches the `initial_environment' list.
    !> Port of env.el:getenv (no FRAME argument).
    subroutine getenv(variable, value, found)
        character(len=*), intent(in) :: variable
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        integer :: stat

        call getenv_internal(variable, value, stat)
        found = stat == GETENV_SET
    end subroutine getenv

    !> Set the value of the environment variable named VARIABLE to VALUE.
    !> VARIABLE should be a string.  VALUE is optional; if not provided,
    !> the environment variable VARIABLE will be removed.
    !>
    !> This function works by modifying `process_environment'.
    !>
    !> If VARIABLE contains `=', an `error' error condition occurs (in
    !> STATUS if present).  Port of env.el:setenv (no
    !> SUBSTITUTE-ENV-VARS).
    subroutine setenv(variable, value, status)
        character(len=*), intent(in) :: variable
        character(len=*), intent(in), optional :: value
        type(status_type), intent(out), optional :: status
        type(status_type) :: st
        integer :: err

        if (index(variable, "=") > 0) then
            call set_error(st, FM_GENERIC_ERROR, &
                           format_message("Environment variable name `")//variable//format_message("' contains `='"))
            if (present(status)) status = st
            return
        end if

        call setenv_internal(variable, .true., value)

        ! The editor's own reads of the environment.
        if (present(value)) then
            err = sys_setenv(variable, value)
        else
            err = sys_unsetenv(variable)
        end if

        if (present(status)) status = st
    end subroutine setenv

    !> `string' with $FOO and ${FOO} replaced by environment variables and
    !> $$ by $.  References to unset variables are left unchanged with
    !> `keep_undefined', else replaced by "".  Port of
    !> env.el:substitute-env-vars (WHEN-UNDEFINED absent or .true.).
    function substitute_env_vars(string, keep_undefined) result(out)
        character(len=*), intent(in) :: string
        logical, intent(in) :: keep_undefined
        character(len=:), allocatable :: out
        character(len=:), allocatable :: value
        integer :: i, j, rbrace
        logical :: found

        out = ""
        i = 1

        do while (i <= len(string))
            if (string(i:i) /= "$" .or. i == len(string)) then
                out = out//string(i:i)
                i = i + 1
                cycle
            end if

            ! env--substitute-vars-regexp: $NAME, ${NAME} or $$
            if (string(i + 1:i + 1) == "$") then
                out = out//"$"
                i = i + 2
            else if (string(i + 1:i + 1) == "{") then
                rbrace = index(string(i + 2:), "}")

                if (rbrace <= 1 .or. index(string(i + 2:i + rbrace), "{") > 0) then
                    out = out//"$"
                    i = i + 1
                    cycle
                end if

                call getenv(string(i + 2:i + rbrace), value, found)

                if (found .or. .not. keep_undefined) then
                    out = out//value
                else
                    out = out//string(i:i + rbrace + 1)
                end if

                i = i + rbrace + 2
            else
                j = i + 1

                do while (j <= len(string))
                    if (.not. name_char(string(j:j))) exit
                    j = j + 1
                end do

                if (j == i + 1) then
                    out = out//"$"
                    i = i + 1
                    cycle
                end if

                call getenv(string(i + 1:j - 1), value, found)

                if (found .or. .not. keep_undefined) then
                    out = out//value
                else
                    out = out//string(i:j - 1)
                end if

                i = j
            end if
        end do

    contains

        !> [[:alnum:]_] (ASCII).
        pure logical function name_char(c)
            character, intent(in) :: c

            name_char = verify(c, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") == 0
        end function name_char
    end function substitute_env_vars
end module env_mod
